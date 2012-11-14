{-
Copyright 2012 Google Inc. All Rights Reserved.

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


import Control.Concurrent (forkIO)
import Control.Monad (replicateM, void)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy (fromChunks)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
#if MIN_VERSION_wai_middleware_route(0, 7, 3)
#else
#endif
import Network.HTTP.Types (Ascii, methodPost, status200)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Route as Route
import qualified Network.Wai as Wai
import System.IO
import System.Posix (sleep)
import System.Random

import Plush.Job
import Plush.Job.Types
import Plush.Run
import Plush.Server.API
import Plush.Server.Utilities
import Plush.Utilities

-- | Run the plush web server. The supplied 'Runner' is used as the shell, and
-- an optional port can be supplied. This action does not complete until the
-- shell exits.
server :: Runner -> Maybe Int -> IO ()
server runner port = do
    (shellThread, origOut, _origErr) <- startShell runner
    key <- genKey
    hPutStrLn origOut $ "Starting server, connect to: " ++ startUrl key
    void $ forkIO $ launchOpen shellThread (openCmd $ startUrl key)
    Warp.run port' $ application shellThread key
  where
    port' = fromMaybe 29544 port
    genKey = replicateM 40 $ randomRIO ('a','z')
    startUrl key =  "http://localhost:" ++ show port' ++ "/index.html#" ++ key
    openCmd url = "xdg-open " ++ url ++ " 2>/dev/null || open " ++ url
    launchOpen st cmd =
        sleep 1 >> submitJob st (CommandRequest "opener" False (CommandItem cmd))

#if MIN_VERSION_wai_middleware_route(0, 7, 3)

application :: ShellThread -> String -> Wai.Application
application shellThread key =
    ($staticApp) $ Route.dispatch True $ Route.mkRoutes'
        [ Route.Post "api/run" $ jsonKeyApp (runApp shellThread)
        , Route.Post "api/poll" $ jsonKeyApp (pollApp shellThread)
        , Route.Post "api/input" $ jsonKeyApp (inputApp shellThread)
        , Route.Post "api/history" $ jsonKeyApp (historyApp shellThread)
        ]
  where
    jsonKeyApp app = jsonApp $ keyedApp key app


#else

-- wai-middleware-route (0, 2, 0)
application :: ShellThread -> String -> Wai.Application
application shellThread key = Route.dispatch
    [ (Route.rule methodPost "^/api/run", jsonKeyApp $ runApp shellThread)
    , (Route.rule methodPost "^/api/poll", jsonKeyApp $ pollApp shellThread)
    , (Route.rule methodPost "^/api/input", jsonKeyApp $ inputApp shellThread)
    , (Route.rule methodPost "^/api/history", jsonKeyApp $ historyApp shellThread)
    ]
    (staticApp $ respApp notFound)
  where
    jsonKeyApp app = jsonApp $ keyedApp key app

#endif

staticApp :: Wai.Middleware
staticApp app req = do
    mbs <- if ok then liftIO (getStaticResource fp) else return Nothing
    case mbs of
        Nothing -> app req
        Just bs -> liftIO . return . resp $ fromChunks [bs]
  where
    pi_ = Wai.pathInfo req
    ok = all (not . T.isPrefixOf ".") pi_
    fp = T.unpack $ T.intercalate "/" pi_
    resp = Wai.responseLBS status200 [("Content-Type", getMimeType fp)]

getMimeType :: FilePath -> Ascii
getMimeType fp = fromMaybe defaultMimeType $ flip lookup defaultMimeTypes
    $ drop 1 $ dropWhile (/= '.') fp

defaultMimeType :: Ascii
defaultMimeType = "application/octet-stream"

defaultMimeTypes :: [(String, Ascii)]
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
