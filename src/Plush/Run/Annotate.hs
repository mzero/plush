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
import Data.List ((\\))
import qualified Data.Map as M
import Data.Maybe (maybeToList)


import Plush.Run.Command
import Plush.Run.Expansion
import Plush.Run.Posix
import Plush.Run.ShellExec
import Plush.Run.Types
import Plush.Types

type Annotations = [(Location, [Annotation])]

annotate :: (PosixLike m) => CommandList -> ShellExec m Annotations
annotate cl = coallesce <$> annoCommandList cl
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
                (fc,ud) <- commandSearch cmd'
                mcs <- getSummary cmd'
                argAnnos <- noteArgs ud args
                return $ noteCommand cmd fc mcs ++ argAnnos
            _ -> return []
        return $ cmdAnno ++ exAnnos

    noteExpansion w = (location w, [ExpandedTo $ wordText w])
    noteCommand cmd fc mcs
        = [(location cmd,
                FoundCommandAnno fc
                : maybeToList (CommandSummaryAnno <$> mcs)
          )]
    noteArgs ud args = getAnnos >>= return . zip locs
      where
        locs = map location args
        getAnnos = utilAnnotate ud $ map quoteRemoval args

    coallesce = M.toAscList . M.fromListWith (++)

