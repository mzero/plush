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

{-# LANGUAGE ForeignFunctionInterface #-}

module Plush.Main (plushMain) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')
import System.Console.GetOpt
import System.Console.Haskeline
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hIsTerminalDevice, stdin)

import Plush.DocTest
import Plush.Run
import Plush.Server
import Plush.Utilities


data Options = Options
                { optMode :: Options -> [String] -> IO ()
                , optRunner :: Runner
                }

optionsDescr :: [OptDescr (Options -> Options)]
optionsDescr =
    [ Option ['?'] ["help"] (NoArg setHelp)     "help (this message)"
    , Option [] ["version"] (NoArg setVersion)  "print vesion and exit"
    , Option ['c'] [] (NoArg setReadArgMode)    "read commands from 1st arg"
    , Option ['s'] [] (NoArg setReadStdinMode)  "read commands from stdin"
    , Option ['w'] ["webserver"] (NoArg setWebServerMode) "run web server"
    , Option ['d'] ["debug"] (NoArg setDebugMode) "debugging commands"
    , Option ['t'] ["test"] (NoArg setTestExec) "execution in test mode"
    ]
  where
    setHelp opts = opts { optMode = (\_ _ -> usage) }
    setVersion opts = opts { optMode = (\_ _ -> version) }
    setReadStdinMode opts = opts { optMode = processStdin }
    setReadArgMode opts = opts { optMode = processArg }
    setWebServerMode opts = opts { optMode = runWebServer }
    setDebugMode opts = opts { optMode = debugOptions }
    setTestExec opts = opts { optRunner = runInTest }

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv =
   case getOpt Permute optionsDescr argv of
      (o,n,[]  ) -> return (foldl' (flip ($)) defaultOpts o,n)
      (_,_,errs) -> usageFailure (concat errs)
  where
    defaultOpts =
        Options { optMode = processFile, optRunner = runInIO }

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn "Usage:"
    putStr $ unlines . map (("  "++prog)++) . lines $
        "                    -- read commands from stdin\n\
        \ -s                 -- read commands from stdin\n\
        \ <file>             -- read commands from file\n\
        \ -c <commands>      -- read commands from argument\n\
        \ -w [<port>]        -- run web server\n\
        \ -d parse           -- interactive parse\n\
        \ -d doctest <file>* -- run doctest over the files\n\
        \ -d shelltest shell <file>* -- run doctest via the given shell\n"
    putStr $ usageInfo ("Options:") optionsDescr

usageFailure :: String -> IO a
usageFailure msg = do
    mapM_ (putStrLn . ("*** " ++)) $ lines msg
    usage
    exitFailure

version :: IO ()
version = putStrLn $ "Plush, the comfy shell, version " ++ displayVersion


foreign export ccall plushMain :: IO ()

plushMain :: IO ()
plushMain = do
    (opts, args) <- getArgs >>= parseOptions
    optMode opts opts args
    exitSuccess

processFile :: Options -> [String] -> IO ()
processFile opts [] = processStdin opts []
processFile opts (fp:args) = readUtf8File fp >>= processArg opts . (:(fp:args))

processArg :: Options -> [String] -> IO ()
processArg _ [] = return ()
processArg opts (cmds:_nameAndArgs) = runCommands cmds (optRunner opts)

processStdin :: Options -> [String] -> IO ()
processStdin opts _args = do
    isTerm <- hIsTerminalDevice stdin
    if isTerm
        then runRepl (optRunner opts)
        else getContents >>= (\cmds -> runCommands cmds (optRunner opts))


runWebServer :: Options -> [String] -> IO ()
runWebServer opts args = case args of
    [] -> serve Nothing
    [port] -> maybe badArgs (serve . Just) $ readMaybe port
    _ -> badArgs
  where
    serve = server (optRunner opts)
    badArgs = usage >> exitFailure

debugOptions :: Options -> [String] -> IO ()
debugOptions _ ["parse"] = runRepl runInPrettyPrint
debugOptions _ ("doctest":fps) = runDocTests fps
debugOptions _ ("shelltest":shell:fps) = shellDocTests shell fps
debugOptions _ _ = usage >> exitFailure

runRepl :: Runner -> IO ()
runRepl = runInputT defaultSettings . repl
  where
    repl runner = do
        l <- getInputLine "# "
        case l of
            Nothing -> return ()
            Just input -> do
                (leftOver, runner') <- liftIO (runCommand input runner)
                when (not $ null leftOver) $
                    outputStrLn ("Didn't use whole input: " ++ leftOver)
                repl runner'

runCommands :: String -> Runner -> IO ()
runCommands "" _ = return ()
runCommands cmds runner = runCommand cmds runner >>= uncurry runCommands

runCommand :: String -> Runner -> IO (String, Runner)
runCommand cmds runner = do
    pr <- runParseCommand cmds runner
    case pr of
        Left errs -> putStrLn errs >> return ("", runner)
        Right (cl, rest) -> runCommandList cl runner
                                >>= return . (\runner' -> (rest, runner'))


