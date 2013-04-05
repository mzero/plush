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

{- | 'OutputStream' creates the abstraction of a stream of some data type
     read from a 'Fd'. The data is parsed off the underlying stream of bytes
     as available. Partially parsed values are handled, and parsing is
     continued as more bytes become available.

     As data is read from an open file descriptor ('Fd'), this interface is
     therefore in 'IO', and the opaque 'OutputStream' contains an 'MVar'.
-}

module Plush.Job.Output (
    OutputStream,
    outputStreamUtf8,
    outputStreamJson,

    getAvailable,
    getOne,
    ) where


import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Concurrent
import Control.Monad (msum, mzero)
import Control.Monad.Exception (catchIOError)
import Data.Aeson (json', Value)
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (createAndTrim)
import System.Posix


-- | The parsing continuation form of a "Data.Attoparsec" parser.
type ParseC a = B.ByteString -> A.Result a

-- | Return as many values as can be parsed off the 'ByteString' given
-- the current parsing continuation. The initial continuation is used when
-- starting a parse of a new value, directly after a previous complete value
-- has been parsed.
parseAvailable :: ParseC a         -- ^ initial continuation
               -> B.ByteString     -- ^ bytes to parse
               -> ParseC a         -- ^ current continuation
               -> (ParseC a, [a])  -- ^ next continuation, and parsed values
parseAvailable p0 = go
  where
    go s p | B.null s = (p, [])
    go s p = case p s of
        A.Fail _s' _ _ -> (p0, [])  -- on error, drop this whole buffer
            -- TODO: during errors we may want to try to resync by
            -- chopping off the first byte of _s' and trying again
        A.Partial f -> (f, [])
        A.Done s' v -> second (v:) $ go s' p0


-- | An initial continuation for parsing UTF-8 bytes as characters
parseUtf8 :: ParseC Char
parseUtf8 = A.parse utf8
    -- Why another implementation of UTF-8 parsing when this executable
    -- already has four other implementations linked into it? Because none
    -- of those implementations exports a continuable parser!
  where
    utf8 = do
        b <- next
        msum $ map ($b) ranges

    ranges =
        [ ifInRange 0x00 0x7f (return . toEnum)
        , ifInRange 0xc2 0xdf (multi 2  0x0080   0x07ff . (-0xc0+))
        , ifInRange 0xe0 0xed (multi 3  0x0800   0xd7ff . (-0xe0+))
        , ifInRange 0xee 0xef (multi 3  0xe000   0xffff . (-0xe0+))
        , ifInRange 0xf0 0xf4 (multi 4 0x10000 0x10ffff . (-0xf0+))
        , const (return '\xfffd')  -- prefer replacement char than error
        ]

    next :: A.Parser Int
    next = fromEnum <$> A.anyWord8

    ifInRange :: Int -> Int -> (Int -> A.Parser a) -> Int -> A.Parser a
    ifInRange lo hi p v = if lo <= v && v <= hi then p v else mzero

    multi :: Int -> Int -> Int -> Int -> A.Parser Char
    multi 1 lo hi v = ifInRange lo hi (return . toEnum) v
    multi n lo hi v =
        next >>= ifInRange 0x80 0xbf
            (multi (n-1) lo hi . ((v*64-0x80)+))


-- | An initial continuation for parsing JSON 'Value's
parseJson :: ParseC Value
parseJson = A.parse json'


-- | A stream of values of type @/a/@, parsed from an open file descriptor.
-- /N.B.:/ This value is stateful (contains an 'MVar').
data OutputStream a = OutputStream Fd (ParseC a) (MVar (ParseC a))

-- | Smart 'OutputStream' constructor
outputStream :: ParseC a -> Fd ->IO (OutputStream a)
outputStream p0 fd = do
    setFdOption fd NonBlockingRead True
    OutputStream fd p0 <$> newMVar p0

-- | Create an 'OutputStream' reading UTF-8 characters
outputStreamUtf8 :: Fd -> IO (OutputStream Char)
outputStreamUtf8 = outputStream parseUtf8

-- | Create an 'OutputStream' reading JSON 'Value's
outputStreamJson :: Fd -> IO (OutputStream Value)
outputStreamJson = outputStream parseJson

-- | Return any available items pulled from an 'OutputStream'
getAvailable :: OutputStream a -> IO [a]
getAvailable (OutputStream fd p0 mv) = do
    s <- readAvailable fd
    modifyMVar mv (\p -> return $ parseAvailable p0 s p)

-- | Return the next available item. Blocks until available via
-- 'threadWaitRead'
getOne :: OutputStream a -> IO a
getOne os@(OutputStream fd _p0 _mv) = do
    as <- getAvailable os
    case as of
        [] -> threadWaitRead fd >> getOne os
        [a] -> return a
        (a:_as') -> return a -- TODO(mzero): should stash as' for future return

-- | Read all available bytes from a file descriptor.
readAvailable :: Fd -> IO B.ByteString -- TODO: should merge with parsing logic
readAvailable fd = go [] >>= return . B.concat . reverse
  where
    go bs = next >>= maybe (return bs) (go . (:bs))
    next = readBuf `catchIOError` (\_ -> return Nothing)
    readBuf = do
      b <- B.createAndTrim bufSize $ (\buf ->
                fromIntegral `fmap` fdReadBuf fd buf bufSize)
      return $ if B.null b then Nothing else Just b
    bufSize :: Num a => a
    bufSize = 4096
