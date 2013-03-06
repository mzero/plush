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

module Plush.DocTest (
    runDocTests,
    shellDocTests,
    )
    where

import Control.Applicative ((<$>))
import Control.Monad (join, when)
import Control.Monad.Trans.Class (lift)
import Data.List (foldl', intercalate, isInfixOf, isPrefixOf, partition)
import System.FilePath (takeFileName)
import System.IO (hFlush, hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Process (readProcessWithExitCode)

import Plush.Run
import Plush.Run.Execute
import Plush.Run.Script
import Plush.Run.TestExec (testOutput)
import Plush.Utilities


data Location = Location { _locFile :: Maybe FilePath, _locLine :: Int }
instance Show Location where
    show (Location mf l) = maybe "" (++ ", ") mf ++ "line " ++ show l

setFile :: FilePath -> Location -> Location
setFile fp (Location _ l) = Location (Just fp) l

data Test = Test
    { testLoc :: Location
    , testInput :: String
    , testExpected :: String
    , testExpectedError :: [String]
    , testCondition :: String -> Bool
    }

newTest :: Int -> Test
newTest i = Test (Location Nothing i) "" "" [] (const True)

noteFile :: FilePath -> Test -> Test
noteFile fp t = t { testLoc = setFile fp $ testLoc t }

inputLine :: String -> Test -> Test
inputLine  s t
    | null (testInput t) = t { testInput = s' ++ "\n", testCondition = cond s }
    | otherwise = t { testInput = testInput t ++ s' ++ "\n" }
  where
    s' = retab s
    cond l = case keyword "SKIP" l of
        Just "" -> const False
        Just r -> not . (`elem` words r)
        Nothing -> case keyword "ONLY" s of
            Just r -> (`elem` words r)
            Nothing -> const True
    keyword _ [] = Nothing
    keyword k l@(_:l') | k `isPrefixOf` l = Just $ drop (length k) l
                       | otherwise = keyword k l'

expectLine :: String -> Test -> Test
expectLine  s t = t { testExpected = testExpected t ++ retab s ++ "\n" }

expectError :: String -> Test -> Test
expectError  s t = t { testExpectedError = testExpectedError t ++ [s] }

retab :: String -> String
retab = map (\c -> if c == '⟶' then '\t' else c)

extractTests :: String -> [Test]
extractTests input = skipping $ zip [1..] $ lines input
  where
    skipping [] = []
    skipping ((i,s):ls) =
        case findInitialPrompt s of
            Nothing -> skipping ls
            Just (leadin, c0) -> command (inputLine c0 $ newTest i) leadin ls

    command test leadin lls@((_,s):ls) =
        case findContinuePrompt leadin s of
            Nothing -> expect test leadin lls
            Just c -> command (inputLine c test) leadin ls
    command test leadin lls = expect test leadin lls

    expect test leadin lls@((_,s):ls) =
        case findExpectLine leadin s of
            Nothing -> test : skipping lls
            Just f -> expect (f test) leadin ls
    expect test _ lls = test : skipping lls

    findInitialPrompt = go ""
      where
        go leadin ('#':' ':s) = Just (leadin, s)
        go leadin (' ':s) = go (' ':leadin) s
        go _ _ = Nothing

    findContinuePrompt leadin s = removeLeadin leadin s >>= go
      where
        go ('+':' ':t) = Just t
        go _ = Nothing

    findExpectLine leadin s = removeLeadin leadin s >>= go
      where
        go "" = Nothing
        go ('.':t) = Just $ expectLine t
        go ('!':' ':t) = Just $ expectError t
        go (c:' ':_) | c == '#' = Nothing
        go t = Just $ expectLine t

    removeLeadin [] s = Just s
    removeLeadin (l:ls) (c:cs) | l == c = removeLeadin ls cs
    removeLeadin _ _ = Nothing

breakTestsAtErrors :: [Test] -> [[Test]]
breakTestsAtErrors = go []
  where
    go [] [] = []
    go qs [] = [qs]
    go qs (t:ts)
        | null (testExpectedError t) = go (qs ++ [t]) ts
        | otherwise                  = (qs ++ [t]) : go [] ts

docTestFile :: FilePath -> IO [Test]
docTestFile fp =
    readUtf8File fp >>= return . map (noteFile fp) . extractTests



divider :: IO ()
divider = putStrLn $ replicate 72 '-'

leftAlign :: Int -> String -> String
leftAlign n s | n <= 0 = s
leftAlign n []         = replicate n ' '
leftAlign n (c:cs)     = c : leftAlign (n - 1) cs

rightAlign :: Int -> String -> String
rightAlign n = reverse . leftAlign n . reverse

data Result = BadParse String
            | UnexpectedResult String
            | UnexpectedError String
            | Skipped
            | Success
    deriving Eq

determineResult :: Test -> String -> String -> Result
determineResult test actualOut actualErr =
    if expectedOut == actualOut
        then if null expectedErrs || any (`isInfixOf` actualErr) expectedErrs
            then Success
            else UnexpectedError actualErr
        else UnexpectedResult actualOut
  where
    expectedOut = testExpected test
    expectedErrs = testExpectedError test


reportResult :: Test -> Result -> IO ()
reportResult test result = case result of
    BadParse errs -> do
        failHeader
        putStrLn "Parse error: "
        putStrIndented errs

    UnexpectedResult actual -> do
        failHeader
        putStrLn "Expecting:"
        putStrIndented $ testExpected test
        putStrLn "Actual:"
        putStrIndented actual

    UnexpectedError actual -> do
        failHeader
        putStrLn "Expected one of these errors:"
        mapM_ putStrIndented $ testExpectedError test
        putStrLn "Actual error:"
        putStrIndented actual

    Skipped -> do
        putStrLn $ "Skipped: " ++ show (testLoc test)

    Success -> return ()

  where
    failHeader = do
        divider
        putStrLn $ "Fail: " ++ show (testLoc test)
        putStrLn "Running:"
        putStrIndented $ testInput test

    putStrIndented = putStr . unlines . map ("    "++) . lines

reportResults :: Bool -> [(Test, Result)] -> IO ()
reportResults verbose trs = if verbose then reportVerbose else reportLight
  where
    reportVerbose = do
        mapM_ (uncurry reportResult) trs'
        divider
        putStrLn $ show sAll ++ " successes, "
            ++ show fAll ++ " failures"
            ++ if kAll > 0 then ", " ++ show kAll ++ " skipped" else ""
    reportLight = do
        putStrLn $ rightAlign 4 (show sAll) ++ " pass "
            ++ rightAlign 4 (show fAll) ++ " fail "
            ++ rightAlign 4 (show kAll) ++ " skip"

    trs' = uncurry (++) $ partition ((/= Skipped).snd) trs
    (sAll,kAll,fAll) = foldl' q (0::Int,0::Int,0::Int) $ map snd trs
    q (s,k,f) Success = (s+1,k,f)
    q (s,k,f) Skipped = (s,k+1,f)
    q (s,k,f) _ = (s,k,f+1)


type RunTests = [Test] -> IO [(Test, Result)]

runAndReport :: String -> Bool -> RunTests -> [FilePath] -> IO ()
runAndReport name verbose testF fps = do
    hSetBuffering stdout NoBuffering
    putStr $ if verbose
        then "=== Running " ++ name ++ " ===\n"
        else leftAlign 48 $ "Running " ++ name ++ "... "
    hFlush stdout
    tests <- mapM docTestFile fps
    results <- mapM testF $ concatMap breakTestsAtErrors tests
    reportResults verbose $ concat results
    when verbose $ putStrLn ""



runTests :: [Test] -> [(Test, Result)]
runTests tests = fst $ foldl' go ([], testRunner) tests
  where
    go (trs, runner) test =
        let (r, runner') = runTest test runner
        in (trs++[(test, r)],runner')

runTest :: Test -> TestRunner -> (Result, TestRunner)
runTest t r0 =
    if testCondition t "plush"
        then let (p, r1) = testRun (parseInput $ testInput t) r0
             in (either badParse exec $ join p) r1
        else (Skipped, r0)
  where
    exec (c,"") r =
        let (result, r') = testRun (execute c >> lift testOutput) r
            (actualOut, actualErr) = either (\e -> (e, "")) id result
        in (determineResult t actualOut actualErr, r')
    exec (_,extra) r = badParse ("Leftover input: " ++ extra) r
    badParse errs r =
        if null (testExpectedError t)
            then (BadParse errs, r)
            else (determineResult t "" errs, r)

runDocTests :: Bool -> [FilePath] -> IO ()
runDocTests verbose =
    runAndReport "doctest (plush test mode)" verbose $ return . runTests




shellTests :: String -> [Test] -> IO [(Test, Result)]
shellTests shcmd tests = do
    ran <- process <$> readProcessWithExitCode shcmd [] script
    let skipped = zip testsToSkip (repeat Skipped)
    return $ ran ++ skipped
  where
    script = intercalate echoBreak $ map testInput testsToRun
    process (_exitCode, actualOuts, actualErrs) =
        zipWith3 process1 testsToRun (splitUp actualOuts) (splitUp actualErrs)
    process1 t o e = (t, determineResult t o e)

    shname = takeFileName shcmd
    (testsToRun, testsToSkip) = partition (\t -> testCondition t shname) tests

    splitUp sIn = go "" sIn ++ repeat ""
      where
        go "" "" = []
        go w "" = [w]
        go w s@(c:cs)
            | breakLn `isPrefixOf` s = w : go "" (drop n s)
            | otherwise = go (w++[c]) cs
        n = length breakLn
    echoBreak = "echo '" ++ breaker ++ "'\n\
                \echo '" ++ breaker ++ "' >&2\n"
    breakLn = breaker ++ "\n"
    breaker = "~~~ ~~~ ~~~ ~~~ ~~~ ~~~"

shellDocTests :: Bool -> String -> [FilePath] -> IO ()
shellDocTests verbose shcmd =
    runAndReport ("shelltest " ++ shcmd) verbose $ shellTests shcmd

