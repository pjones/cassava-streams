{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
       ( StreamDecodingError (..)
       , decodeStream
       , decodeStreamWith
       , decodeStreamByName
       , decodeStreamByNameWith
       , onlyValidRecords
       ) where

--------------------------------------------------------------------------------
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Csv hiding (Parser, decodeWith, decodeByNameWith)
import Data.Csv.Incremental
import Data.IORef
import Data.Typeable
import System.IO.Streams (InputStream, makeInputStream)
import qualified System.IO.Streams as Streams

--------------------------------------------------------------------------------
-- | Exception thrown when stream decoding cannot continue due to an
-- error.
newtype StreamDecodingError = StreamDecodingError String
  deriving (Typeable, Show)

instance Exception StreamDecodingError

--------------------------------------------------------------------------------
-- | Create an @InputStream@ which decodes CSV records from the given
-- upstream data source.
--
-- Equivalent to @decodeStreamWith defaultDecodeOptions@.
decodeStream :: (FromRecord a)
             => HasHeader
             -- ^ Whether to skip a header or not.
             -> InputStream ByteString
             -- ^ Upstream.
             -> IO (InputStream (Either String a))
             -- ^ An @InputStream@ which produces records.
decodeStream = decodeStreamWith defaultDecodeOptions

--------------------------------------------------------------------------------
-- | Create an @InputStream@ which decodes CSV records from the given
-- upstream data source.
decodeStreamWith :: (FromRecord a)
                 => DecodeOptions
                 -- ^ CSV decoding options.
                 -> HasHeader
                 -- ^ Whether to skip a header or not.
                 -> InputStream ByteString
                 -- ^ Upstream.
                 -> IO (InputStream (Either String a))
                 -- ^ An @InputStream@ which produces records.
decodeStreamWith ops hdr input = do
  queue  <- newIORef []
  parser <- newIORef $ Just (decodeWith ops hdr)
  makeInputStream (dispatch queue parser input)

--------------------------------------------------------------------------------
-- | Create an @InputStream@ which decodes CSV records from the given
-- upstream data source.  Data should be preceded by a header.
--
-- Equivalent to @decodeStreamByNameWith defaultDecodeOptions@.
decodeStreamByName :: (FromNamedRecord a)
                   => InputStream ByteString
                   -- ^ Upstream.
                   -> IO (InputStream (Either String a))
                   -- ^ An @InputStream@ which produces records.
decodeStreamByName = decodeStreamByNameWith defaultDecodeOptions

--------------------------------------------------------------------------------
-- | Create an @InputStream@ which decodes CSV records from the given
-- upstream data source.  Data should be preceded by a header.
decodeStreamByNameWith :: (FromNamedRecord a)
                       => DecodeOptions
                       -- ^ CSV decoding options.
                       -> InputStream ByteString
                       -- ^ Upstream.
                       -> IO (InputStream (Either String a))
                       -- ^ An @InputStream@ which produces records.
decodeStreamByNameWith ops input = go (decodeByNameWith ops) where
  -- Dispatch on the HeaderParser type.
  go (FailH _ e)  = bomb e
  go (PartialH f) = Streams.read input >>= go . maybe (f BS.empty) f
  go (DoneH _ p)  = do
    queue  <- newIORef []
    parser <- newIORef (Just p)
    makeInputStream (dispatch queue parser input)

--------------------------------------------------------------------------------
-- | Creates a new @InputStream@ which only sends valid CSV records
-- downstream.  The first invalid record will throw an exception.
onlyValidRecords :: InputStream (Either String a)
                 -- ^ Upstream.
                 -> IO (InputStream a)
                 -- ^ An @InputStream@ which only produces valid
                 -- records.
onlyValidRecords input = makeInputStream $ do
  upstream <- Streams.read input

  case upstream of
    Nothing         -> return Nothing
    Just (Left err) -> bomb ("invalid CSV row: " ++ err)
    Just (Right x)  -> return (Just x)

--------------------------------------------------------------------------------
-- | Internal function which feeds data to the CSV parser.
dispatch :: forall a. IORef [Either String a]
         -- ^ List of queued CSV records.
         -> IORef (Maybe (Parser a))
         -- ^ Current CSV parser state.
         -> InputStream ByteString
         -- ^ Upstream.
         -> IO (Maybe (Either String a))
         -- ^ Data feed downstream.
dispatch queueRef parserRef input = do
  queue <- readIORef queueRef

  case queue of
    [] -> do
      parser <- readIORef parserRef
      case parser of
        Nothing          -> return Nothing
        Just (Fail _  e)  -> bomb ("input data malformed: " ++ e)
        Just (Many [] f) -> more f
        Just (Many xs f) -> writeIORef parserRef (Just $ Many [] f) >> feed xs
        Just (Done xs  ) -> writeIORef parserRef Nothing            >> feed xs

    (x:xs) -> do
      writeIORef queueRef xs
      return (Just x)

  where
    -- Send more data to the CSV parser.  If there is no more data
    -- from upstream then send an empty @ByteString@.
    more :: (ByteString -> Parser a) -> IO (Maybe (Either String a))
    more f = do bs <- Streams.read input
                writeIORef parserRef (Just $ maybe (f BS.empty) f bs)
                dispatch queueRef parserRef input

    -- Feed records downstream.
    feed :: [Either String a] -> IO (Maybe (Either String a))
    feed xs = do writeIORef queueRef xs
                 dispatch queueRef parserRef input

--------------------------------------------------------------------------------
-- | Throw an exception.
bomb :: String -> IO a
bomb = throwIO . StreamDecodingError
