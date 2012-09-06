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
import Data.Maybe (fromMaybe)
#if MIN_VERSION_wai_middleware_route(0, 7, 3)
#else
import qualified Data.Text as T
import Network.HTTP.Types (methodPost)
#endif
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Route as Route
import qualified Network.Wai.Middleware.Static as Static
import qualified Network.Wai as Wai
import System.FilePath
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
    staticPath <- (</> "static") `fmap` getDataDir
    key <- genKey
    hPutStrLn origOut $ "Starting server, connect to: " ++ startUrl key
    void $ forkIO $ launchOpen shellThread (openCmd $ startUrl key)
    Warp.run port' $ application shellThread key staticPath
  where
    port' = fromMaybe 29544 port
    genKey = replicateM 40 $ randomRIO ('a','z')
    startUrl key =  "http://localhost:" ++ show port' ++ "/index.html#" ++ key
    openCmd url = "xdg-open " ++ url ++ " 2>/dev/null || open " ++ url
    launchOpen st cmd =
        sleep 1 >> submitJob st (CommandRequest "opener" False (CommandItem cmd))

#if MIN_VERSION_wai_middleware_route(0, 7, 3)

application :: ShellThread -> String -> FilePath -> Wai.Application
application shellThread key staticPath =
    ($staticApp) $ Route.dispatch True $ Route.mkRoutes'
        [ Route.Post "api/run" $ jsonKeyApp (runApp shellThread)
        , Route.Post "api/poll" $ jsonKeyApp (pollApp shellThread)
        , Route.Post "api/input" $ jsonKeyApp (inputApp shellThread)
        , Route.Post "api/history" $ jsonKeyApp (historyApp shellThread)
        ]
  where
    jsonKeyApp app = jsonApp $ keyedApp key app
    staticApp = (Static.staticPolicy $ Static.addBase staticPath)
        (respApp notFound)

#else

-- wai-middleware-route (0, 2, 0)
application :: ShellThread -> String -> FilePath -> Wai.Application
application shellThread key staticPath = Route.dispatch
    [ (Route.rule methodPost "^/api/run", jsonKeyApp $ runApp shellThread)
    , (Route.rule methodPost "^/api/poll", jsonKeyApp $ pollApp shellThread)
    , (Route.rule methodPost "^/api/input", jsonKeyApp $ inputApp shellThread)
    , (Route.rule methodPost "^/api/history", jsonKeyApp $ historyApp shellThread)
    ]
    staticApp
  where
    jsonKeyApp app = jsonApp $ keyedApp key app
    staticApp = (Static.staticRoot (T.pack staticPath)) (respApp notFound)

#endif
