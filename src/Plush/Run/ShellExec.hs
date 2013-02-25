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

{-# Language TypeSynonymInstances, TypeFamilies, FlexibleInstances,
    GeneralizedNewtypeDeriving #-}

module Plush.Run.ShellExec (
    VarScope(..), VarMode(..), VarEntry,
    ShellState(), initialShellState,
    ShellExec, runShellExec,
    getName, setName,
    getArgs, setArgs,
    getFlags, setFlags,
    varValue, getVars, getVar, getVarDefault,
    getVarEntry, setVarEntry, unsetVarEntry,
    getFun, setFun, unsetFun, withFunContext,
    getAliases, setAliases,
    getEnv,
    getLastExitCode, setLastExitCode,
    getSummary, loadSummaries,
    primeShellState,
    subshell,
    )
    where

import Control.Applicative
import Control.Monad (join, msum, mplus)
import Control.Monad.Exception (bracket_, MonadException(..))
import Control.Monad.Trans.Class (lift, MonadTrans)
import qualified Control.Monad.Trans.State as ST
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T

import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.ShellFlags
import Plush.Run.Types
import Plush.Types
import Plush.Types.CommandSummary
import Plush.Utilities (readMaybe)

-- Shell State

data VarScope = VarShellOnly | VarExported  deriving (Eq, Ord, Bounded, Enum)
data VarMode = VarReadWrite | VarReadOnly   deriving (Eq, Ord, Bounded, Enum)
type VarEntry = (VarScope, VarMode, Maybe String)
type Vars = M.HashMap String VarEntry
type Funs = M.HashMap String FunctionBody
type Aliases = M.HashMap String String

varValue :: VarEntry -> String
varValue (_,_,s) = fromMaybe "" s

data ShellState = ShellState
    { ssShellPid :: Int   -- the pid of this shell
    , ssName :: String    -- the command name used to invoke the shell or script
    , ssArgs :: [String]  -- "Positional Parameters", technically
    , ssFlags :: Flags
    , ssVars :: Vars
    , ssFuns :: Funs
    , ssAliases :: Aliases
    , ssLastExitCode :: ExitCode
    , ssSummaries :: M.HashMap String CommandSummary
    }
    -- TODO: eventually also aliases, functions, async. pids

initialShellState :: ShellState
initialShellState = ShellState
    { ssShellPid = 0
    , ssName = "plush"
    , ssArgs = []
    , ssFlags = defaultFlags
    , ssVars = M.empty
    , ssFuns = M.empty
    , ssAliases = M.empty
    , ssLastExitCode = ExitSuccess
    , ssSummaries = M.empty
    }

-- Shell Execution Monad

newtype ShellExec m a = ShellExec (ST.StateT ShellState m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadException)

runShellExec :: (Monad m) => ShellExec m a -> ShellState -> m (a, ShellState)
runShellExec (ShellExec st) = ST.runStateT st

evalShell :: (Monad m) => ShellExec m a -> ShellState -> m a
evalShell (ShellExec st) = ST.evalStateT st

get :: (Monad m) => ShellExec m ShellState
get = ShellExec ST.get

gets :: (Monad m) => (ShellState -> a) -> ShellExec m a
gets f = ShellExec $ ST.gets f

put :: (Monad m) => ShellState -> ShellExec m ()
put s = ShellExec $ ST.put s

modify :: (Monad m) => (ShellState -> ShellState) -> ShellExec m ()
modify f = ShellExec $ ST.modify f

getName :: (Monad m) => ShellExec m String
getName = gets ssName

setName :: (Monad m) => String -> ShellExec m ()
setName name = modify $ \s -> s { ssName = name }

getArgs :: (Monad m) => ShellExec m [String]
getArgs = gets ssArgs

setArgs :: (Monad m) => [String] -> ShellExec m ()
setArgs args = modify $ \s -> s { ssArgs = args }

getFlags :: (Monad m) => ShellExec m Flags
getFlags = gets ssFlags

setFlags :: (Monad m) => Flags -> ShellExec m ()
setFlags flags = modify $ \s -> s { ssFlags = flags }

getVars :: (Monad m, Functor m) => ShellExec m Vars
getVars = gets ssVars

getVar :: (Monad m, Functor m) => String -> ShellExec m (Maybe String)
getVar name = gets varOptions
  where
    varOptions s = join $ msum [normalVar s, specialVar s, positionalVar s]
    normalVar s = (Just . varValue) `fmap` M.lookup name (ssVars s)
    specialVar s = ($s) `fmap` M.lookup name specialVarActions
    positionalVar s = case readMaybe name of
        Just n | n > 0 -> Just . listToMaybe . drop (n - 1) $ ssArgs s
        _ -> Nothing

    specialVarActions = M.fromList
        [ ("#", Just . show . length . ssArgs)
        , ("?", Just . show . exitCodeToInt . ssLastExitCode)
        , ("-", Just . flagParameter . ssFlags)
        , ("$", Just . show . ssShellPid)
        , ("!", const Nothing)
        , ("0", Just . ssName)
        ] -- NOTE: "@" and "*" are handled in Expansion.hs

getVarDefault :: (Monad m, Functor m) => String -> String -> ShellExec m String
getVarDefault name def = fromMaybe def `fmap` getVar name

getVarEntry :: (Monad m) => String -> ShellExec m (Maybe VarEntry)
getVarEntry name = gets ssVars >>= return . M.lookup name

setVarEntry :: (PosixLike m) => String -> VarEntry -> ShellExec m ShellStatus
setVarEntry name entry = do
    curr <- getVarEntry name
    case combineVarEntries curr entry of
        Just newVarEntry -> do
            modify $ \s -> s { ssVars = M.insert name newVarEntry $ ssVars s }
            success
        Nothing ->
            shellError 1 $ "var is read-only: " ++ name
    -- TODO: shouldn't be able to set specials or positionals

-- | combineVarEntries maybeExisting newVarEntry
combineVarEntries :: Maybe VarEntry -> VarEntry -> Maybe VarEntry

-- No old value found:
combineVarEntries Nothing newVarEntry = Just newVarEntry

-- Staying in the RW domain:
combineVarEntries (Just (VarShellOnly, VarReadWrite, old)) (newScope, VarReadWrite, new) =
    Just (newScope, VarReadWrite, new `mplus` old)
combineVarEntries (Just (VarExported, VarReadWrite, old)) (_, VarReadWrite, new) =
    Just (VarExported, VarReadWrite, new `mplus` old)

-- Setting values and making RO:
combineVarEntries (Just (VarShellOnly, VarReadWrite, old)) (VarShellOnly, VarReadOnly, new) =
    Just (VarShellOnly, VarReadOnly, new `mplus` old)
combineVarEntries (Just (VarExported, VarReadWrite, old)) (_, VarReadOnly, new) =
    Just (VarExported, VarReadOnly, new `mplus` old)

-- Annotating existing values as RO:
combineVarEntries old@(Just (_, VarReadOnly, _)) (VarShellOnly, VarReadOnly, Nothing) =
    old
combineVarEntries (Just (_, VarReadOnly, old)) (VarExported, _, Nothing) =
    Just (VarExported, VarReadOnly, old)

-- Otherwise fail:
combineVarEntries _ _ = Nothing

unsetVarEntry :: (PosixLike m) => String -> ShellExec m ShellStatus
unsetVarEntry name = do
    curr <- getVarEntry name
    case curr of
        Nothing -> success
        Just (_, VarReadWrite, _) -> do
            modify $ \s -> s { ssVars = M.delete name $ ssVars s }
            success
        _ ->
            shellError 1 $ "var is read-only: " ++ name

getEnv :: (Monad m, Functor m) => ShellExec m Bindings
getEnv = getVars >>= return . foldr getBinding [] . M.toList
  where
    getBinding (name, ve@(VarExported, _, _)) acc = (name, varValue ve) : acc
    getBinding _ acc = acc

getFun :: (Monad m) => String -> ShellExec m (Maybe FunctionBody)
getFun fname = gets ssFuns >>= return . M.lookup fname

setFun :: (Monad m) => String -> FunctionBody -> ShellExec m ()
setFun fname fun =
    modify $ \s -> s { ssFuns = M.insert fname fun $ ssFuns s }

unsetFun :: (Monad m) => String -> ShellExec m ()
unsetFun fname = modify $ \s -> s { ssFuns = M.delete fname $ ssFuns s }

withFunContext :: (PosixLike m) => [String] -> ShellExec m a -> ShellExec m a
withFunContext args body = do
    origArgs <- gets ssArgs
    bracket_
        (modify $ \s -> s { ssArgs = args })
        (modify $ \s -> s { ssArgs = origArgs })
        body

getAliases :: (Monad m, Functor m) => ShellExec m Aliases
getAliases = gets ssAliases

setAliases :: (Monad m) => Aliases -> ShellExec m ()
setAliases aliases = modify $ \s -> s { ssAliases = aliases }

getLastExitCode :: (Monad m) => ShellExec m ExitCode
getLastExitCode = gets ssLastExitCode

setLastExitCode :: (Monad m) => ExitCode -> ShellExec m ()
setLastExitCode ec = modify $ \s -> s { ssLastExitCode = ec }

getSummary :: (Monad m, Functor m) =>
    String -> ShellExec m (Maybe CommandSummary)
getSummary c = M.lookup c `fmap` gets ssSummaries

loadSummaries :: (Monad m, Functor m) => T.Text -> ShellExec m ()
loadSummaries t = modify $
    \s -> s { ssSummaries = M.fromList $ parseSummaries t }

primeShellState :: (PosixLike m) => ShellExec m ()
primeShellState = do
    pid <- getProcessID
    e <- map asVar `fmap` getInitialEnvironment
    modify $ \s ->
        s { ssShellPid = pid, ssVars = M.fromList e `M.union` ssVars s }
  where
    asVar (var, val) = (var,(VarExported, VarReadWrite, Just val))

subshell :: (PosixLike m) => ShellExec m a -> ShellExec m a
subshell act = do
    s <- get
    wd <- getWorkingDirectory
    bracket_ (return ()) (changeWorkingDirectory wd >> put s) act
    -- TODO(mzero): needs to restore umask and traps once those are implemented,
    -- also, once exec redirection can leave files open/redirected, those need
    -- to be restored as well


liftT1 :: (Monad m, MonadTrans t) => (a -> m r) -> a -> t m r
liftT2 :: (Monad m, MonadTrans t) => (a -> b -> m r) -> a -> b -> t m r
liftT3 :: (Monad m, MonadTrans t) =>
                (a -> b -> c -> m r) -> a -> b -> c -> t m r
liftT4 :: (Monad m, MonadTrans t) =>
                (a -> b -> c -> d -> m r) -> a -> b -> c -> d -> t m r
liftT1 f a = lift $ f a
liftT2 f a b = lift $ f a b
liftT3 f a b c = lift $ f a b c
liftT4 f a b c d = lift $ f a b c d


instance PosixLike m => PosixLike (ShellExec m) where
    createDirectory = liftT2 Plush.Run.Posix.createDirectory
    removeDirectory = liftT1 removeDirectory

    getDirectoryContents = liftT1 getDirectoryContents

    getWorkingDirectory = lift getWorkingDirectory
    changeWorkingDirectory = liftT1 changeWorkingDirectory

    getInitialEnvironment = lift getInitialEnvironment

    type FileStatus (ShellExec m) = FileStatus m

    getFileStatus = liftT1 getFileStatus
    getSymbolicLinkStatus = liftT1 getSymbolicLinkStatus
    isExecutable = liftT1 isExecutable

    removeLink = liftT1 removeLink

    setFileTimes = liftT3 setFileTimes
    touchFile = liftT1 touchFile

    openFd = liftT4 openFd
    createFile = liftT2 createFile
    closeFd = liftT1 closeFd

    readAll = liftT1 readAll
    write = liftT2 write

    dupTo = liftT2 dupTo
    dupFdCloseOnExec = liftT2 dupFdCloseOnExec
    setCloseOnExec = liftT1 setCloseOnExec

    getUserHomeDirectoryForName = liftT1 getUserHomeDirectoryForName
    realAndEffectiveIDsMatch = lift realAndEffectiveIDsMatch

    getProcessID = lift getProcessID
    execProcess = liftT4 execProcess
    captureStdout a = get >>= lift . captureStdout . evalShell a

    pipeline cs = do
        s <- get
        lift $ pipeline [ evalShell c s | c <- cs ]
    contentFd = liftT1 contentFd

