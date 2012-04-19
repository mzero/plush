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

module Plush.Run.ShellFlags (
    -- * Flags
    Flags(..),
    defaultFlags,
    defaultInteractiveFlags,

    -- * Operations
    flagParameter,
    processFlagArgs,

    -- * Flag Descriptions
    FlagDesc(..),
    flagDescriptions
    ) where


import Data.Maybe (listToMaybe, mapMaybe)


-- | Shell flags, as defined in §14, set special built-in.
--
-- All of the Posix flags are defined here even though most of them are not
-- yet implemented.
--
-- N.B.: The flags have canonical names and are reproduced here as the record
-- selectors of Flags. Hence, this module should generally be imported
-- qualified.
data Flags = Flags
    { allexport   :: Bool  -- -a
    , errexit     :: Bool  -- -e
    , hashall     :: Bool  -- -h
    , ignoreeof   :: Bool
    , interactive :: Bool  -- -i, not part of the set command as per spec.
    , monitor     :: Bool  -- -m
    , noclobber   :: Bool  -- -C
    , noexec      :: Bool  -- -n
    , noglob      :: Bool  -- -f
    , nolog       :: Bool
    , notify      :: Bool  -- -b
    , nounset     :: Bool  -- -u
    , verbose     :: Bool  -- -v
    , vi          :: Bool
    , xtrace      :: Bool  -- -x
    }

-- | All flags default to off
defaultFlags :: Flags
defaultFlags = Flags
    { allexport   = False
    , errexit     = False
    , hashall     = False
    , ignoreeof   = False
    , interactive = False
    , monitor     = False
    , noclobber   = False
    , noexec      = False
    , noglob      = False
    , nolog       = False
    , notify      = False
    , nounset     = False
    , verbose     = False
    , vi          = False
    , xtrace      = False
    }

-- | For interactive shells, interactive (-i) and monitor (-m) are on by default
defaultInteractiveFlags :: Flags
defaultInteractiveFlags = defaultFlags { interactive = True, monitor = True }

-- | Returns set flags as a string of their short, single character names,
-- as specified for special parameter @$-@ (§2.5.2). Flags without short names
-- are not included.
flagParameter :: Flags -> String
flagParameter flags = mapMaybe fdShortName setFlags
  where
    setFlags = filter (\d -> fdGetter d flags) flagDescriptions

-- | The flag arguments are removed and converted, as a whole, into a function
-- that sets and clears flags. The remaining non-flag arguments are returned.
-- Non-flag arguments includes all arguments after a @\"--\"@ argument.
processFlagArgs :: [String] -> (Flags -> Flags, [String])
processFlagArgs = go
  where
    go as@("--":_) = (id,as)
    go ("-o":n:as) = long n True `andThen` go as
    go ("+o":n:as) = long n False `andThen` go as
    go (('-':cs):as) = short cs True `andThen` go as
    go (('+':cs):as) = short cs False `andThen` go as
    go as = (id,as)

    andThen :: (a->a,[b]) -> (a->a,[b]) -> (a->a,[b])
    (f,as) `andThen` (g,bs) = (g.f,as++bs)

    long :: String -> Bool -> (Flags -> Flags, [String])
    long n b = maybeSet [oflag b,n] b $ findLong n

    short :: String -> Bool -> (Flags -> Flags, [String])
    short cs b = let (f,cs') = foldr (andThen.sf) (id,"") cs
                 in (f, if null cs' then [] else [pflag b:cs'])
      where
        sf c = maybeSet [c] b $ findShort c

    maybeSet :: [a] -> Bool -> Maybe FlagDesc -> (Flags -> Flags, [a])
    maybeSet r b = maybe (id,r) (\d -> (fdSetter d b,[]))

    oflag b = pflag b : "o"
    pflag True = '-'
    pflag False = '+'

    findLong n = find ((== n) . fdLongName)
    findShort c = find (maybe False (== c) . fdShortName)
    find p = listToMaybe $ filter p flagDescriptions



-- | Describes a shell flag.
data FlagDesc = FlagDesc
    { fdShortName :: Maybe Char
    , fdLongName :: String -- in spec. neither -h nor -i has a long name
    , fdGetter :: Flags -> Bool
    , fdSetter :: Bool -> Flags -> Flags
    }

-- | Describes all the shell flags.
flagDescriptions =
    [ FlagDesc (Just 'a') "allexport"   allexport   (\b f->f{ allexport = b })
    , FlagDesc (Just 'e') "errexit"     errexit     (\b f->f{ errexit = b })
    , FlagDesc (Just 'h') "hashall"     hashall     (\b f->f{ hashall = b })
        -- the long name is not in the spec., probably an omission
    , FlagDesc Nothing    "ignoreeof"   ignoreeof   (\b f->f{ ignoreeof = b })
    , FlagDesc (Just 'i') "interactive" interactive (\b f->f{ interactive = b })
        -- not part of the set command in the spec, only args to the shell
    , FlagDesc (Just 'm') "monitor"     monitor     (\b f->f{ monitor = b })
    , FlagDesc (Just 'C') "noclobber"   noclobber   (\b f->f{ noclobber = b })
    , FlagDesc (Just 'n') "noexec"      noexec      (\b f->f{ noexec = b })
    , FlagDesc (Just 'f') "noglob"      noglob      (\b f->f{ noglob = b })
    , FlagDesc Nothing    "nolog"       nolog       (\b f->f{ nolog = b })
    , FlagDesc (Just 'b') "notify"      notify      (\b f->f{ notify = b })
    , FlagDesc (Just 'u') "nounset"     nounset     (\b f->f{ nounset = b })
    , FlagDesc (Just 'v') "verbose"     verbose     (\b f->f{ verbose = b })
    , FlagDesc Nothing    "vi"          vi          (\b f->f{ vi = b })
    , FlagDesc (Just 'x') "xtrace"      xtrace      (\b f->f{ xtrace = b })
    ]

