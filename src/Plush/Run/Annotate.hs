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

module Plush.Run.Annotate (
    Annotation(..),
    Annotations,
    annotate,
    )
where

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Control.Monad.Error.Class
import Data.List ((\\), isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (catMaybes, maybeToList)
import System.FilePath


import Plush.Run.Command
import Plush.Run.Expansion
import Plush.Run.Posix
import Plush.Run.ShellExec
import Plush.Run.Types
import Plush.Types

type Annotations = [(Location, [Annotation])]

annotate :: (PosixLike m) => CommandList -> Maybe Int -> ShellExec m Annotations
annotate cl cursor = coallesce <$> annoCommandList cl
  where
    annoCommandList aos = concat <$> mapM annoCommandItem aos
    annoCommandItem (ao, _) = annoAndOr ao

    annoAndOr aos = concat <$> mapM annoAndOrItem aos
    annoAndOrItem (_, (_, p)) = annoPipe p

    annoPipe cs = concat <$> mapM annoCommand cs
    annoCommand (Command ws _ _) = do
        exws <- expandPassive ws
        let exAnnos = map noteExpansion $ exws \\ ws
        cmdAnno <- case exws of
            (cmd:args) -> do
                let cmd' = quoteRemoval cmd
                (fc, _, an) <- commandSearch cmd'
                mcs <- getSummary cmd'
                argAnnos <- noteArgs an args
                return $ noteCommand cmd fc mcs ++ argAnnos
            _ -> return []
        compAnnos <- case ws of
            (cmd:args) -> do
                cmdComp <- noteCmdCompletion cmd
                argComps <- mapM noteCompletion args
                return $ catMaybes $ cmdComp : argComps
            []         -> return []
        return $ cmdAnno ++ exAnnos ++ compAnnos

    noteExpansion w = (location w, [ExpandedTo $ wordText w])
    noteCommand cmd fc mcs
        = [(location cmd,
                FoundCommandAnno fc
                : maybeToList (CommandSummaryAnno <$> mcs)
          )]
    noteArgs an args = getAnnos >>= return . zip locs
      where
        locs = map location args
        getAnnos = an $ map quoteRemoval args

    noteCompletion w = case (location w, cursor) of
        (Span s e, Just c) | (s - 1) <= c && c < e -> do
            comps <- compFiles . splitAt (c - s + 1) $ wordText w
            return $ Just (location w, [Completions comps])
        _ -> return Nothing
      where
        compFiles (pre, post) = map (++post) <$> (pathnameGlob $ pre ++ "*")

    noteCmdCompletion w
        | '/' `elem` wordText w = noteCompletion w
        | otherwise             = case (location w, cursor) of
            (Span s e, Just c) | (s - 1) <= c && c < e -> do
                comps <- cmdComp . splitAt (c - s + 1) $ wordText w
                return $ Just (location w, [Completions comps])
            _ -> return Nothing
      where
        cmdComp (pre, post) = map (++post) <$> (prefixMatchOnPath pre)
        prefixMatchOnPath p = do
          pathFiles <- allPathFiles
          matchingPaths <- filterM (execAndPrefix p) pathFiles
          return $ map takeFileName matchingPaths
        execAndPrefix p x =
            if isFilePrefixOf p x
                then isExecutable x `catchError` (\_ -> return False)
                else return False
        isFilePrefixOf p x = p `isPrefixOf` takeFileName x
        allPathFiles = do
            path <- getVarDefault "PATH" ""
            pathFiles <- mapM safeGetDirectoryPaths $ splitSearchPath path
            return $ concat pathFiles
        safeGetDirectoryPaths p = do
            files <- getDirectoryContents p `catchError` (\_ -> return [])
            return $ map (p </>) files

    coallesce = M.toAscList . M.fromListWith (++)
