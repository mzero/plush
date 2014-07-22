{-
Copyright 2012-2013 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, CPP #-}

module Plush.Server.WebServer (
    startLocal,
    )
    where

import Control.Applicative ((<$>))
import Control.Concurrent (myThreadId)
import qualified Control.Exception as Ex
import Control.Monad (replicateM)
import Data.Aeson (encode, fromJSON, Result(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Data.Text as T
#if MIN_VERSION_wai_middleware_route(0, 7, 3)
#else
import qualified Data.Text.Encoding as T
#endif
import Data.Typeable (Typeable)
import qualified Network.HTTP.Types as H
import Network.Socket (socketPort)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Route as Route
import System.Posix
import System.Random

import Plush.Job
import Plush.Job.Output
import Plush.Resource
import Plush.Run
import Plush.Run.Types
import Plush.Run.Posix.Utilities (writeStrLn)
import Plush.Server.API
import Plush.Server.Status
import Plush.Server.Utilities
import qualified Plush.Server.Warp as Warp'


-- | Start a local plush server by detaching the server process.
--
-- The server process communicates its success or failure at starting up on
-- a pipe that is created between it and the parent process running this
-- action.
startLocal :: IO Runner -> Maybe Int -> IO (Either String ServerInfo)
startLocal mkRunner mPort = do
    (mP, sP) <- createPipe
    outerPid <- forkProcess $ do
        _ <- createSession
        closeFd mP
        _ <- dupTo sP stdInput
        _ <- dupTo sP stdOutput
        _ <- dupTo sP stdError
        closeFd sP
        _ <- forkProcess $
            server mkRunner mPort
                $ \fd si -> writeStrLn fd (encode si)
        exitImmediately ExitSuccess
    closeFd sP
    _ <- getProcessStatus True False outerPid
    rs <- outputStreamJson mP >>= getOne
    closeFd mP
    return $ case fromJSON rs of
        Error m -> Left $ "Server process response parse error:\n" ++ m
        Success (Left m) -> Left $ "Server process start-up error:\n" ++ m
        Success (Right si) -> Right si


data ExitServer = ExitServer
  deriving (Show, Typeable)

instance Ex.Exception ExitServer


-- | Run the plush web server. The supplied 'Runner' is used as the shell, and
-- an optional port can be supplied. This action does not complete until the
-- shell exits.
server :: IO Runner -> Maybe Int
    -> (Fd -> Either String ServerInfo -> IO ())
    -> IO ()
server mkRunner port reportInfo = do
    (shellThread, origOutFd, _origErrFd) <- startShell mkRunner
    Ex.handle (reportStartFailure origOutFd) $ do
        key <- genKey
        pid <- getProcessID
        Ex.bracket bindSock closeSock  $ \socket -> do
            installSignalHandlers
            port' <- fromIntegral <$> socketPort socket
            let si = ServerInfo
                        { siType = LocalServer
                        , siPid = pid
                        , siPort = port'
                        , siKey = key
                        }
            logger <- makeLogger si
            reportStart origOutFd logger si
                -- At this point we are committed to running the server, as
                -- we've reported that it has started. This following call to
                --  `handle` ensures that any subsequent exception (including
                --  exit) will be logged.
            Ex.handle (logExit logger) $
                Ex.bracket_ (writeLocalServerInfo si) remoteLocalServerInfo $
                    Warp'.runSettingsSocket (settings port' logger) socket
                        $ dispatchApp (jsonApis shellThread key)
                        $ staticApp
                        $ respApp notFound
  where
    bindSock = Warp'.bindServerSocket port (fromString "127.0.0.1")
    closeSock = Warp'.closeServerSocket
    settings port' logger = Warp.defaultSettings
        { Warp.settingsPort = port'
        , Warp.settingsHost = fromString "127.0.0.1"
        , Warp.settingsOnException = logError logger
        }
    genKey = replicateM 40 $ randomRIO ('a','z')

    reportStartFailure origOutFd e =
        reportInfo origOutFd $ Left $ show (e :: Ex.SomeException)

    reportStart :: Fd -> (String -> IO ()) -> ServerInfo -> IO ()
    reportStart origOutFd logger si = do
        logger ("start, port " ++ show (siPort si))
        reportInfo origOutFd $ Right si

    installSignalHandlers = do
        tid <- myThreadId
        let catcher = Catch $ Ex.throwTo tid ExitServer
        mapM_ (\s -> installHandler s catcher Nothing)
            [keyboardSignal, keyboardTermination, softwareTermination]
            -- TODO(mzero): are these the right set of signals to catch?

    logError logger _mreq err
        | Ex.fromException err == Just Ex.ThreadKilled = return ()
        | otherwise = case Ex.fromException err :: Maybe Warp.InvalidRequest of
            Just e -> logger $ "invalid request: " ++ show e
            Nothing -> logger $ "exception: " ++ show err

    logExit logger err = do
        logger $ case (Ex.fromException err :: Maybe ExitServer) of
            Just _ -> "exit"
            Nothing -> "abnormal exit: " ++ show err

    writeLocalServerInfo = writeServerInfo LocalServer
    remoteLocalServerInfo = removeServerInfo LocalServer


#if MIN_VERSION_wai_middleware_route(0, 7, 3)

dispatchApp :: [(T.Text, Wai.Application)] -> Wai.Middleware
dispatchApp = Route.dispatch True . Route.mkRoutes' . map (uncurry Route.Post)

#else

-- wai-middleware-route (0, 2, 0)
dispatchApp :: [(T.Text, Wai.Application)] -> Wai.Middleware
dispatchApp = Route.dispatch . map rte
  where
    rte (p,a) = (Route.rule H.methodPost (T.encodeUtf8 $ T.append "^/" p), a)

#endif

jsonApis :: ShellThread -> String -> [(T.Text, Wai.Application)]
jsonApis shellThread key =
    [ ("api/run", jsonKeyApp runApp)
    , ("api/poll", jsonKeyApp pollApp)
    , ("api/input", jsonKeyApp inputApp)
    , ("api/history",  jsonKeyApp historyApp)
    ]
  where
    jsonKeyApp j = jsonApp $ keyedApp key $ j shellThread


staticApp :: Wai.Middleware
staticApp app req respond = do
    mbs <- if ok then getStaticResource fp else return Nothing
    case mbs of
        Nothing -> app req respond
        Just bs -> respond . resp $ LBS.fromChunks [bs]
  where
    pi_ = Wai.pathInfo req
    ok = all (not . T.isPrefixOf ".") pi_
    fp = T.unpack $ T.intercalate "/" pi_
    resp = Wai.responseLBS H.status200 [("Content-Type", getMimeType fp)]

getMimeType :: FilePath -> BS.ByteString
getMimeType fp = fromMaybe defaultMimeType $ flip lookup defaultMimeTypes
    $ drop 1 $ dropWhile (/= '.') fp

defaultMimeType :: BS.ByteString
defaultMimeType = "application/octet-stream"

defaultMimeTypes :: [(String, BS.ByteString)]
defaultMimeTypes =
    [ ("css",     "text/css")
    , ("gif",     "image/gif")
    , ("html",    "text/html")
    , ("jpeg",    "image/jpeg")
    , ("jpg",     "image/jpeg")
    , ("js",      "text/javascript")
    , ("ogg",     "application/ogg")
    , ("png",     "image/png")
    ]
