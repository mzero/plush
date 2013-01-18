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

{-# LANGUAGE OverloadedStrings, CPP #-}

module Plush.Server (
    server,
    )
    where

import Control.Applicative ((<$>), (<|>))
import Control.Concurrent (forkIO)
import qualified Control.Exception as Ex
import Control.Monad (replicateM, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Network (HostPreference(Host))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
#if MIN_VERSION_wai_middleware_route(0, 7, 3)
#else
import qualified Data.Text.Encoding as T
#endif
import Data.Time (getZonedTime)
import qualified Network.HTTP.Types as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Route as Route
import System.Posix (Fd, sleep)
import System.Random

import Plush.Job
import Plush.Job.Types
import Plush.Run
import Plush.Run.Posix.Utilities (writeStr)
import Plush.Server.API
import Plush.Server.Utilities
import qualified Plush.Server.Warp as Warp'
import Plush.Utilities

-- | Run the plush web server. The supplied 'Runner' is used as the shell, and
-- an optional port can be supplied. This action does not complete until the
-- shell exits.
server :: Runner -> Maybe Int -> IO ()
server runner port = do
    (shellThread, origOutFd, origErrFd) <- startShell runner
    key <- genKey
    writeStr origOutFd $ "Starting server, connect to: " ++ startUrl key
    void $ forkIO $ launchOpen shellThread (openCmd $ startUrl key)
    Warp'.runSettings (settings origErrFd)
        $ dispatchApp (jsonApis shellThread key)
        $ staticApp
        $ respApp notFound
  where
    port' = fromMaybe 29544 port
    settings errOut = Warp.defaultSettings
        { Warp.settingsPort = port'
        , Warp.settingsHost = Host "127.0.0.1"
        , Warp.settingsOnException = reportError errOut
        }
    genKey = replicateM 40 $ randomRIO ('a','z')
    startUrl key =  "http://localhost:" ++ show port' ++ "/index.html#" ++ key
    openCmd url = "xdg-open " ++ url ++ " 2>/dev/null || open " ++ url
    launchOpen st cmd =
        sleep 1 >> submitJob st (CommandRequest "opener" False (CommandItem cmd))


reportError :: Fd -> Ex.SomeException -> IO ()
reportError errOut err = logger $
    (case e of { Just Ex.ThreadKilled -> Just Nothing; _ -> Nothing })
    <|> (Just . ("Invalid Request: " ++) . show) <$> (e :: Maybe Warp.InvalidRequest)
    <|> (Just . Just $ "Exception: " ++ show err)
  where
    e :: (Ex.Exception a) => Maybe a
    e = Ex.fromException err

    logger (Just (Just s)) = do
        t <- show <$> getZonedTime
        writeStr errOut $ t ++ ' ' : s
    logger _ = return ()

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
staticApp app req = do
    mbs <- if ok then liftIO (getStaticResource fp) else return Nothing
    case mbs of
        Nothing -> app req
        Just bs -> liftIO . return . resp $ LBS.fromChunks [bs]
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
