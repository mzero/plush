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

{-# Language TypeSynonymInstances, TypeFamilies, FlexibleInstances #-}

module Plush.Run.ShellExec (
    VarScope(..), VarMode(..), VarEntry,
    ShellState(), initialShellState,
    ShellExec,
    getArgs, setArgs,
    getFlags, setFlags,
    getVars, getVar, getVarDefault, setVarEntry,
    getLastExitCode, setLastExitCode,
    getSummary, loadSummaries,
    primeShellState,

    ShellUtility,
    )
    where

import Control.Monad.Error
import Control.Monad.Trans.State
import qualified Data.HashMap.Strict as M
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Plush.Run.Posix
import Plush.Run.ShellFlags
import Plush.Run.Types
import Plush.Types.CommandSummary

-- Shell State

data VarScope = VarShellOnly | VarExported  deriving (Eq, Ord, Bounded, Enum)
data VarMode = VarReadWrite | VarReadOnly   deriving (Eq, Ord, Bounded, Enum)
type VarEntry = (VarScope, VarMode, String)
type Vars = M.HashMap String VarEntry

varValue :: VarEntry -> String
varValue (_,_,s) = s

data ShellState = ShellState
    { ssName :: String    -- the command name used to invoke the shell or script
    , ssArgs :: [String]  -- "Positional Parameters", technically
    , ssFlags :: Flags
    , ssVars :: Vars
    , ssLastExitCode :: ExitCode
    , ssSummaries :: M.HashMap String CommandSummary
    }
    -- TODO: eventually also aliases, functions, async. pids

initialShellState :: ShellState
initialShellState = ShellState
    { ssName = "plush", ssArgs = [], ssFlags = defaultFlags, ssVars = M.empty,
        ssLastExitCode = ExitSuccess, ssSummaries = M.empty }


-- Shell Execution Monad

type ShellExec m = StateT ShellState m

getArgs :: (Monad m) => ShellExec m [String]
getArgs = gets ssArgs

setArgs :: (Monad m) => [String] -> ShellExec m ()
setArgs args = modify $ (\s -> s { ssArgs = args })

getFlags :: (Monad m) => ShellExec m Flags
getFlags = gets ssFlags

setFlags :: (Monad m) => Flags -> ShellExec m ()
setFlags flags = modify $ (\s -> s { ssFlags = flags })

getVars :: (Monad m, Functor m) => ShellExec m Vars
getVars = gets ssVars

getVar :: (Monad m, Functor m) => String -> ShellExec m (Maybe String)
getVar name = get >>= (\s -> return . msum $ [normalVar s, specialVar s])
  where
    normalVar s = varValue `fmap` M.lookup name (ssVars s)
    specialVar s = case name of
        "*" -> Just . intercalate (starSep s) . ssArgs $ s
        "#" -> Just . show . length . ssArgs $ s
        "?" -> Just . show . exitStatus . ssLastExitCode $ s
        "-" -> Just . flagParameter . ssFlags $ s
        _ -> Nothing
    starSep = take 1 . maybe " " varValue . M.lookup "IFS" . ssVars
    exitStatus ExitSuccess = 0
    exitStatus (ExitFailure n) = n

getVarDefault :: (Monad m, Functor m) => String -> String -> ShellExec m String
getVarDefault name def = fromMaybe def `fmap` getVar name

setVarEntry :: (Monad m) => String -> VarEntry -> ShellExec m ()
setVarEntry name entry = modify $
    (\s -> s { ssVars = M.insert name entry $ ssVars s })
    -- TODO: shouldn't be able to set specials or positionals
    -- TODO: shouldn't be able to set readonly vars

getLastExitCode :: (Monad m) => ShellExec m ExitCode
getLastExitCode = gets ssLastExitCode

setLastExitCode :: (Monad m) => ExitCode -> ShellExec m ()
setLastExitCode ec = modify $ (\s -> s { ssLastExitCode = ec })

getSummary :: (Monad m, Functor m) =>
    String -> ShellExec m (Maybe CommandSummary)
getSummary c = M.lookup c `fmap` gets ssSummaries

loadSummaries :: (Monad m, Functor m) => T.Text -> ShellExec m ()
loadSummaries t = modify $
    \s -> s { ssSummaries = M.fromList $ parseSummaries t }

primeShellState :: (PosixLike m) => ShellExec m ()
primeShellState = do
    e <- map asVar `fmap` getEnvironment
    modify $ (\s -> s { ssVars = M.fromList e `M.union` ssVars s })
  where
    asVar (var, val) = (var,(VarExported, VarReadWrite, val))


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

    getEnvironment = lift getEnvironment

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

    rawSystem = liftT2 rawSystem
    pipeline cs = do
        s <- get
        lift $ pipeline [ evalStateT c s | c <- cs ]


type ShellUtility m = Utility (ShellExec m)
