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
module System.IO.Streams.Csv.Encode
       ( encodeStream
       , encodeStreamWith
       , encodeStreamByName
       , encodeStreamByNameWith
       ) where

--------------------------------------------------------------------------------
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.IORef
import System.IO.Streams (OutputStream, makeOutputStream)
import qualified System.IO.Streams as Streams

--------------------------------------------------------------------------------
-- | Create a new @OutputStream@ that can be fed @ToRecord@ values
-- which are converted to CSV.  The records are encoded into
-- @ByteString@s and passed on to the given downstream @OutputStream@.
--
-- Equivalent to @encodeStreamWith defaultEncodeOptions@.
encodeStream :: ToRecord a
             => OutputStream ByteString -- ^ Downstream.
             -> IO (OutputStream a)     -- ^ New @OutputStream@.
encodeStream = encodeStreamWith defaultEncodeOptions

--------------------------------------------------------------------------------
-- | Create a new @OutputStream@ that can be fed @ToRecord@ values
-- which are converted to CSV.  The records are encoded into
-- @ByteString@s and passed on to the given downstream @OutputStream@.
encodeStreamWith :: ToRecord a
                 => EncodeOptions           -- ^ Encoding options.
                 -> OutputStream ByteString -- ^ Downstream.
                 -> IO (OutputStream a)     -- ^ New @OutputStream@.
encodeStreamWith opts output = do
  ref <- newIORef opts
  makeOutputStream (dispatch encodeWith ref output)

--------------------------------------------------------------------------------
-- | Create a new @OutputStream@ which can be fed @ToNamedRecord@
-- values that will be converted into CSV.  The records are encoded
-- into @ByteString@s and passed on to the given downstream
-- @OutputStream@.
--
-- Equivalent to @encodeStreamByNameWith defaultEncodeOptions@.
encodeStreamByName :: ToNamedRecord a
                   => Header                   -- ^ CSV Header.
                   -> OutputStream ByteString  -- ^ Downstream.
                   -> IO (OutputStream a)      -- ^ New @OutputStream@.
encodeStreamByName = encodeStreamByNameWith defaultEncodeOptions

--------------------------------------------------------------------------------
-- | Create a new @OutputStream@ which can be fed @ToNamedRecord@
-- values that will be converted into CSV.  The records are encoded
-- into @ByteString@s and passed on to the given downstream
-- @OutputStream@.
encodeStreamByNameWith :: ToNamedRecord a
                       => EncodeOptions            -- ^ Encoding options.
                       -> Header                   -- ^ CSV Header.
                       -> OutputStream ByteString  -- ^ Downstream.
                       -> IO (OutputStream a)      -- ^ New @OutputStream@.
encodeStreamByNameWith opts hdr output = do
  ref <- newIORef opts
  makeOutputStream $ dispatch (\opts' -> encodeByNameWith opts' hdr) ref output

--------------------------------------------------------------------------------
-- | Encode records, ensuring that the header is written no more than once.
dispatch :: (EncodeOptions -> [a] -> BL.ByteString) -- ^ Encoding function.
         -> IORef EncodeOptions                     -- ^ Encoding options.
         -> OutputStream ByteString                 -- ^ Downstream.
         -> Maybe a                                 -- ^ Record to write.
         -> IO ()
dispatch _   _   output Nothing  = Streams.write Nothing output
dispatch enc ref output (Just x) = do
  opts <- readIORef ref
  when (encIncludeHeader opts) $ writeIORef ref (opts {encIncludeHeader = False})
  Streams.writeLazyByteString (enc opts [x]) output
