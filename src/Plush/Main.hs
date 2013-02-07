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

{-# LANGUAGE ForeignFunctionInterface #-}

module Plush.Main (plushMain) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import System.Console.Haskeline
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess, exitWith)
import System.IO (Handle, hIsTerminalDevice, hPutStr, hPutStrLn,
    stderr, stdin, stdout)
import System.FilePath ((</>))
import System.Posix.Files (ownerModes)
import System.Posix.Missing (getArg0)

import Plush.ArgParser
import Plush.DocTest
import Plush.Resource
import Plush.Run
import Plush.Run.Expansion
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.Script
import qualified Plush.Run.ShellExec as Shell
import qualified Plush.Run.ShellFlags as F
import Plush.Server
import Plush.Utilities


foreign export ccall plushMain :: IO ()

plushMain :: IO ()
plushMain = do
    progName <- getArg0
    fullArgs <- getArgs
    let (flagF, nonFlagArgs) = F.processFlagArgs fullArgs
    case mconcat $ processArgs commandLineOptions nonFlagArgs of
        (OA (Right (optF, args))) -> do
            let opts = optF $ Options
                        { optMode = processFile
                        , optRunner = runnerInIO
                        , optLogin = take 1 progName == "-"
                        , optShellName = progName
                        , optSetFlags = flagF
                        , optShellArgs = []
                        }
            optMode opts opts args
            exitSuccess
        (OA (Left err)) -> usageFailure err

data Options = Options
                { optMode :: Options -> [String] -> IO ()
                , optRunner :: Runner
                , optLogin :: Bool
                , optShellName :: String
                , optSetFlags :: F.Flags -> F.Flags
                , optShellArgs :: [String]
                }

commandLineOptions :: [OptionSpec Options]
commandLineOptions =
    [ OptionSpec ['?'] ["help"]       (NoArg $ setMode help)
    , OptionSpec []    ["version"]    (NoArg $ setMode version)
    , OptionSpec ['c'] []             (NoArg $ setMode processArg)
    , OptionSpec ['s'] []             (NoArg $ setMode processStdin)
    , OptionSpec ['w'] ["webserver"]  (NoArg $ setMode runWebServer)
    , OptionSpec []    ["doctest"]    (NoArg $ setMode doctest)
    , OptionSpec []    ["shelltest"]  (NoArg $ setMode shelltest)
    , OptionSpec []    ["login"]      (NoArg $ setLogin True)
    , OptionSpec ['t'] ["test"]       (NoArg $ setRunner runnerInTest)
    ]
  where
    setMode m opts = opts { optMode = m }
    setRunner r opts = opts { optRunner = r }
    setLogin b opts = opts { optLogin = b }


usage :: Handle -> IO ()
usage h = do
    prog <- getProgName
    hPutStrLn h "Usage:"
    hPutStr h $ unlines . map (("  "++prog)++) . lines $
        "                    -- read commands from stdin\n\
        \ -s                 -- read commands from stdin\n\
        \ <file>             -- read commands from file\n\
        \ -c <commands>      -- read commands from argument\n\
        \ -w [<port>]        -- run web server\n\
        \ --doctest <file>*  -- run doctest over the files\n\
        \ --shelltest shell <file>* -- run doctest via the given shell\n"

usageFailure :: String -> IO a
usageFailure msg = do
    mapM_ (putStrLn . ("*** " ++)) $ lines msg
    usage stderr
    exitFailure


initialRunner :: Options -> IO Runner
initialRunner opts = do
    scriptDefaults <- makeScriptDefaults
    fmap snd $ run (setupShell opts scriptDefaults) (optRunner opts)
  where
    makeScriptDefaults = if (optLogin opts)
        then (,) <$> getDataResource "profile" <*> getDataResource "env"
        else return (Nothing, Nothing)

initialInteractiveRunner :: Options -> IO Runner
initialInteractiveRunner opts =
    initialRunner $ opts { optSetFlags = optSetFlags opts . setInteractive }
  where
    setInteractive flags = flags { F.interactive = True }


type ScriptDefaults = (Maybe B.ByteString, Maybe B.ByteString)

setupShell :: (PosixLike m) => Options -> ScriptDefaults -> Shell.ShellExec m ()
setupShell opts (defProfile, defEnv) = do
    Shell.setName $ optShellName opts
    Shell.setFlags $ optSetFlags opts $ baseFlags
    Shell.setArgs $ optShellArgs opts
    when (optLogin opts) $ do
        home <- Shell.getVarDefault "HOME" ""
        when (not $ null home) $ do
            let dotPlush = home </> ".plush"
            createDirIfMissing dotPlush
            createScriptIfMissing defProfile $ dotPlush </> "profile"
            createScriptIfMissing defEnv $ dotPlush </> "env"
            maybeRunFile $ Just "/etc/profile"
            maybeRunFile $ Just $ dotPlush </> "profile"
    when iIsSet $ do
        match <- realAndEffectiveIDsMatch
        when match $ do
            getExpandedVar "ENV" >>= maybeRunFile
  where
    iIsSet = F.interactive $ optSetFlags opts $ F.defaultFlags
    baseFlags = if iIsSet
                    then F.defaultInteractiveFlags
                    else F.defaultFlags
        -- N.B.: Interactive as determined at the command line chooses a base
        -- set of flags, rather than setting just the interactive flag.

    maybeRunFile = maybe (return ()) (void . runFile)

    createDirIfMissing fp = do
        exists <- doesDirectoryExist fp
        unless exists $ createDirectory fp ownerModes

    createScriptIfMissing Nothing _ = return ()
    createScriptIfMissing (Just content) fp = do
        exists <- doesFileExist fp
        unless exists $ writeAllFile fp content

--
-- Information Modes
--

help :: Options -> [String] -> IO ()
help _ _ = usage stdout

version :: Options -> [String] -> IO ()
version _ _ = putStrLn $ "Plush, the comfy shell, version " ++ displayVersion


--
-- POSIX Modes
--

-- | Default mode if no mode specified. Handles invocations like:
--
-- > plush
-- > plush <command_file> [<argument>...]
--
processFile :: Options -> [String] -> IO ()
processFile opts [] = processStdin opts []
processFile opts (fp:args) = do
    script <- readUtf8File fp
    processScriptNameArgs opts script (fp:args)
    -- NOTE: This doesn't use runFile, as the file needs to come from the
    -- invoking world, not within the PosixLike environment, which might be
    -- a test environment.

-- | Mode to run command specified on command line. Handles invocations like:
--
-- > plush -c <command_string> [<command_name> [<argument>...]]
--
processArg :: Options -> [String] -> IO ()
processArg _ [] = usageFailure "no command given"
processArg opts (cmds:nameAndArgs) =
    processScriptNameArgs opts cmds nameAndArgs

-- | Mode to run commands from @stdin@. Handles invocations like:
--
-- > plush
-- > plush -s [<argument>...]
--
processStdin :: Options -> [String] -> IO ()
processStdin opts args = do
    isInTerm <- hIsTerminalDevice stdin
    isErrTerm <- hIsTerminalDevice stderr
    if isInTerm && isErrTerm
        then initialInteractiveRunner opts' >>= runRepl
        else getContents >>= processScript opts'
  where
    opts' = opts { optShellArgs = args }


processScriptNameArgs :: Options -> String -> [String] -> IO ()
processScriptNameArgs opts script []            = processScript opts script
processScriptNameArgs opts script (name:args)   = processScript opts' script
  where
    opts' = opts { optShellName = name, optShellArgs = args }

processScript :: Options -> String -> IO ()
processScript opts script = do
    r <- initialRunner opts
    (ec, _) <- run (runScript script) r
    exitWith ec

--
-- Web & Server Modes
--

runWebServer :: Options -> [String] -> IO ()
runWebServer opts args = case args of
    [] -> serve Nothing
    [port] -> maybe badPort (serve . Just) $ readMaybe port
    _ -> usageFailure "too many arguments"
  where
    serve mp = initialInteractiveRunner opts' >>= flip server mp
    badPort = usageFailure "not a port number"
    opts' = opts { optLogin = True }

--
-- Testing Modes
--

doctest :: Options -> [String] -> IO ()
doctest opts fps = runDocTests (isVerbose opts) fps

shelltest :: Options -> [String] -> IO ()
shelltest opts (shell:fps) = shellDocTests (isVerbose opts) shell fps
shelltest _ _ = usageFailure "requires a shell arg"

-- | Repurpose the @-v@ shell flag (@verbose@) as the verbose flag for tests.
isVerbose :: Options -> Bool
isVerbose opts = F.verbose $ optSetFlags opts F.defaultFlags


--
-- Running Commands
--

runRepl :: Runner -> IO ()
runRepl = runInputT defaultSettings . repl
    -- TODO(mzero): This opens a FD on /dev/tty which then leaks thru every
    -- fork and exec. This is a potential resource leak and security risk.
    -- Haskeline has no way to work around this.
    -- See http://trac.haskell.org/haskeline/ticket/123
  where
    repl runner = do
        (ps1, runner') <- liftIO (run prompt runner)
        l <- getInputLine ps1
        case l of
            Nothing -> return ()
            Just input -> do
                (s, runner'') <- liftIO (run (runCommand input) runner')
                case s of
                    (_, Just leftOver) | not $ null leftOver ->
                        outputStrLn ("Didn't use whole input: " ++ leftOver)
                    _ -> return ()
                repl runner''

    prompt :: (PosixLike m) => Shell.ShellExec m String
    prompt = getExpandedVarDefault "PS1" "$ "

--
-- Utility
--

getExpandedVar :: (PosixLike m) => String -> Shell.ShellExec m (Maybe String)
getExpandedVar name = do
    val <- Shell.getVar name
    case val of
        Nothing -> return Nothing
        Just s -> Just <$> contentExpansion s

getExpandedVarDefault :: (PosixLike m) =>
    String -> String -> Shell.ShellExec m String
getExpandedVarDefault name def = fromMaybe def <$> getExpandedVar name
