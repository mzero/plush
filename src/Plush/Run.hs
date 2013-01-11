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

{-# LANGUAGE Rank2Types #-}

module Plush.Run (
    -- * Runner
    Runner,
    run,
    runParseCommand,
    runCommandList,
    runInIO,
    runInTest,
    runInPrettyPrint,

    -- * TestRunner
    TestRunner,
    testRun,
    testRunParseCommand,
    testRunCommandList,
    testRunInTest,
    )
where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import qualified Control.Exception as Ex
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (runStateT)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import Plush.Parser
import Plush.Pretty
import Plush.Resource
import Plush.Run.Execute
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.ShellExec
import qualified Plush.Run.ShellFlags as F
import Plush.Run.TestExec
import Plush.Types


-- | Encapsulates the state and 'PosixLike' monad of a running shell.
-- Using a 'Runner', 'ShellExec' actions can be run, which will result in
-- another 'Runner' being returned.
--
-- Note that since the underlying monad might, or might not, be 'IO':
--
-- 1. Running actions takes place in 'IO', whether or not the underlying monad
-- is 'IO'.
--
-- 2. Reusing a 'Runner' will reuse the prior shell state, but effects on
-- the world may or may not persist.
data Runner = RunPrimed Runner
            | RunInIO ShellState
            | RunInTest ShellState TestState
            | RunInPrettyPrint

-- | Run actions in the @'ShellExec' 'IO'@ monad.
runInIO :: Runner
runInIO = RunPrimed $ RunInIO initialShellState

-- | Run actions in the @'ShellExec' 'TestExec'@ monad.
runInTest :: Runner
runInTest = RunPrimed $ RunInTest initialShellState initialTestState

-- | Run actions under pretty print. This only works with 'runCommandList',
-- which will output the pretty-printed version of the command to @stdout@.
-- It will simply throw an error with arbitrary 'ShellExec' actions. Admittedly,
-- this is a bit of a hack.
runInPrettyPrint :: Runner
runInPrettyPrint = RunInPrettyPrint

-- | Using the encapsulated state and monad in a 'Runner', run a 'ShellExec'
-- action. The result is a pair of the result of the action, and a new 'Runner'
-- encapsulating the updated shell and world state.
--
-- The additional @forall m.@ enforces that you can only give this function
-- actions that are polymorphic in the inner 'PosixLike' monad @m@.
run :: (forall m. PosixLike m => ShellExec m a) -> Runner -> IO (a, Runner)
run act runner = case runner of
    RunPrimed r -> prime r >>= run act

    RunInIO s -> runStateT act s >>= (\(a,s') -> return (a, RunInIO s'))

    RunInTest s t -> do
        let (r,t') = runTest (runStateT act s) t
        (a, s') <- either Ex.throwIO return r
        let (oe, t'') = runTest testOutput t'
        putStr $ either show (uncurry (++)) oe
        return (a, RunInTest s' t'')

    RunInPrettyPrint -> fail "run: can't execute actions in PrettyPrint"

-- | Parse a command, using the state of the 'Runner'
runParseCommand :: String -> Runner -> IO ParseCommandResult
runParseCommand s RunInPrettyPrint = return $ parseCommand M.empty s
runParseCommand s r = fst <$> run (parseInput s) r

-- | Run a 'CommandList' with a 'Runner'
runCommandList :: CommandList -> Runner -> IO Runner
runCommandList cl RunInPrettyPrint = putStrLn (pp cl) >> return RunInPrettyPrint
runCommandList cl r = snd `fmap` run (shellExec cl) r

-- | Initialize the state of the shell for a runner. This requires reading
-- auxillary data from plush's data dir, which is used to set up the internal
-- state of the shell. Therefore, this priming takes place in IO, even if the
-- runner is in Test.
prime :: Runner -> IO Runner
prime r = do
    summaries <- maybe T.empty (T.decodeUtf8With T.lenientDecode)
                    `fmap` getDataResource "summaries.txt"
    snd `fmap` run (primeShellState >> loadSummaries summaries) r

-- | A similar encapsulation as 'Runner', but always with the 'TestExec' monad.
-- As a consequence, actions are not run in the 'IO' monad.
data TestRunner = TestRunPrimed TestRunner
                | TestRunInTest ShellState TestState

-- | Run actions in the @'ShellExec' 'TestExec'@ monad.
testRunInTest :: TestRunner
testRunInTest = TestRunPrimed $ TestRunInTest initialShellState initialTestState

-- | Using the encapsulated state in a 'TestRunner', run a 'ShellExec' action.
-- The result is a pair of the result of the action, and a new 'TestRunner'
-- encapsulating the updated shell and world state.
testRun :: ShellExec TestExec a -> TestRunner -> (Either String a, TestRunner)
testRun act runner = case runner of
    TestRunPrimed r -> testRun (primeShellState >> act) r

    TestRunInTest s t ->
        let (r, t') = runTest (runStateT act s) t
            (r', s') = either (\e -> (Left $ show e, s)) (first Right) r
        in  (r', TestRunInTest s' t')

-- | Parse a command, using the state of the 'TestRunner'
testRunParseCommand :: String -> TestRunner -> ParseCommandResult
testRunParseCommand s r = either Left id . fst $ testRun (parseInput s) r


-- | Run a 'CommandList' with a 'TestRunner'. The first part of the return pair
-- is a combination of the @stdout@ and @stderr@ outputs.
testRunCommandList ::
    CommandList -> TestRunner -> ((String, String), TestRunner)
testRunCommandList cl r = first handleTestError runResult
  where
    runResult = testRun (shellExec cl >> lift testOutput) r
    handleTestError = either (\e -> ("",e)) id


-- | Parse a command, in the state of the shell. Extracts the aliases from
-- the shell state to give to the parser. Also, checks and implements the
-- verbse (-v) and parseout (-P) shell flags.
parseInput :: (PosixLike m) => String -> ShellExec m ParseCommandResult
parseInput s = do
    flags <- getFlags
    when (F.verbose flags) $ errStrLn s
    pcr <- getAliases >>= return . flip parseCommand s
    when (F.parseout flags) $ case pcr of
        Right (cl, _) -> errStrLn $ pp cl
        _ -> return ()
    return pcr
