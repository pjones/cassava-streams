{-

This file is part of the Haskell package cassava-streams. It is
subject to the license terms in the LICENSE file found in the
top-level directory of this distribution and at
git://pmade.com/cassava-streams/LICENSE. No part of cassava-streams
package, including this file, may be copied, modified, propagated, or
distributed except according to the terms contained in the LICENSE
file.

-}

--------------------------------------------------------------------------------
module System.IO.Streams.Csv.Decode
       ( decodeStream
       , decodeStreamWith
       , decodeStreamByName
       , decodeStreamByNameWith
       ) where

--------------------------------------------------------------------------------
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Csv hiding (Parser, decodeWith, decodeByNameWith)
import Data.Csv.Incremental
import Data.Either (lefts, rights)
import Data.IORef
import System.IO.Streams (InputStream, makeInputStream)
import qualified System.IO.Streams as Streams

--------------------------------------------------------------------------------
-- | Create an @InputStream@ which decodes CSV records from the given
-- upstream data source.
--
-- Equivalent to @decodeStreamWith defaultDecodeOptions@.
decodeStream :: (FromRecord a)
             => HasHeader              -- ^ Whether to skip a header or not.
             -> InputStream ByteString -- ^ Upstream.
             -> IO (InputStream a)     -- ^ An @InputStream@ which produces records.
decodeStream = decodeStreamWith defaultDecodeOptions

--------------------------------------------------------------------------------
-- | Create an @InputStream@ which decodes CSV records from the given
-- upstream data source.
decodeStreamWith :: (FromRecord a)
                 => DecodeOptions           -- ^ CSV decoding options.
                 -> HasHeader               -- ^ Whether to skip a header or not.
                 -> InputStream ByteString  -- ^ Upstream.
                 -> IO (InputStream a)      -- ^ An @InputStream@ which produces records.
decodeStreamWith ops hdr input = do
  queue  <- newIORef []
  parser <- newIORef (decodeWith ops hdr)
  makeInputStream (dispatch queue parser input)

--------------------------------------------------------------------------------
-- | Create an @InputStream@ which decodes CSV records from the given
-- upstream data source.  Data should be preceded by a header.
--
-- Equivalent to @decodeStreamByNameWith defaultDecodeOptions@.
decodeStreamByName :: (FromNamedRecord a)
                   => InputStream ByteString -- ^ Upstream.
                   -> IO (InputStream a)     -- ^ An @InputStream@ which produces records.
decodeStreamByName = decodeStreamByNameWith defaultDecodeOptions

--------------------------------------------------------------------------------
-- | Create an @InputStream@ which decodes CSV records from the given
-- upstream data source.  Data should be preceded by a header.
decodeStreamByNameWith :: (FromNamedRecord a)
                       => DecodeOptions          -- ^ CSV decoding options.
                       -> InputStream ByteString -- ^ Upstream.
                       -> IO (InputStream a)     -- ^ An @InputStream@ which produces records.
decodeStreamByNameWith ops input = go (decodeByNameWith ops) where
  -- Dispatch on the HeaderParser type.
  go (FailH _ e)  = bomb e
  go (PartialH f) = Streams.read input >>= go . maybe (f BS.empty) f
  go (DoneH _ p)  = do
    queue  <- newIORef []
    parser <- newIORef p
    makeInputStream (dispatch queue parser input)

--------------------------------------------------------------------------------
-- | Internal function which feeds data to the CSV parser.
dispatch :: IORef [a]              -- ^ List of queued CSV records.
         -> IORef (Parser a)       -- ^ Current CSV parser state.
         -> InputStream ByteString -- ^ Upstream.
         -> IO (Maybe a)           -- ^ Data feed downstream.
dispatch queueRef parserRef input = do
  queue <- readIORef queueRef

  case queue of
    [] -> do
      parser <- readIORef parserRef
      case parser of
        Fail _  e -> bomb e
        Many xs f -> more f >> feed xs
        Done xs   -> feed xs

    (x:xs) -> do
      writeIORef queueRef xs
      return (Just x)

  where
    -- Send more data to the CSV parser.  If there is no more data
    -- from upstream then send an empty @ByteString@.
    more f = do
      bytes <- Streams.read input
      writeIORef parserRef (maybe (f BS.empty) f bytes)

    -- Feed more data downstream or fail if some records didn't parse
    -- correctly.  The elements are wrapped in an @Either@ which
    -- indicates decoding failures.
    feed xs = if not $ null (lefts xs)
                then bomb $ concat (lefts xs)
                else writeIORef queueRef (rights xs) >>
                     dispatch queueRef parserRef input

--------------------------------------------------------------------------------
-- | FIXME: use a proper exception.
bomb :: String -> IO a
bomb = fail
