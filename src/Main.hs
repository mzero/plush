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

module Main (main) where

import Plush.Main

-- | This is never actually called. See src/main.c for the actual main function.
-- that function also calls 'plushMain', but does so after some preparation of
-- the execution state prior to GHC's RTS initialization.
--
-- This file is required, as cabal passes a single Haskell file to GHC's -make
-- mode, and that needs a "Main" module. The GHC flag -no-hs-main is used to
-- ensure that GHC doesn't output a C main function, and ours can be linked in
-- without conflict.
main :: IO ()
main = plushMain
