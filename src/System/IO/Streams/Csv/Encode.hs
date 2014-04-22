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
       , encodeStreamByName
       ) where

--------------------------------------------------------------------------------
import Data.ByteString (ByteString)
import Data.Csv
import System.IO.Streams (OutputStream, makeOutputStream)
import qualified System.IO.Streams as Streams

--------------------------------------------------------------------------------
-- | Create a new @OutputStream@ that can be fed @ToRecord@ values
-- which are converted to CSV.  The records are encoded into
-- @ByteString@s and passed on to the given downstream @OutputStream@.
encodeStream :: ToRecord a
             => OutputStream ByteString -- ^ Downstream.
             -> IO (OutputStream a)     -- ^ New @OutputStream@.
encodeStream output = makeOutputStream stream where
  stream Nothing  = Streams.write Nothing output
  stream (Just x) = Streams.writeLazyByteString (encode [x]) output

--------------------------------------------------------------------------------
-- | Create a new @OutputStream@ that can be fed @ToNamedRecord@
-- values which are converted to CSV.  The records are encoded into
-- @ByteString@s and passed on to the given downstream @OutputStream@.
encodeStreamByName :: ToNamedRecord a
                   => Header                   -- ^ CSV Header.
                   -> OutputStream ByteString  -- ^ Downstream.
                   -> IO (OutputStream a)      -- ^ New @OutputStream@.
encodeStreamByName hdr output = makeOutputStream stream where
  stream Nothing  = Streams.write Nothing output
  stream (Just x) = Streams.writeLazyByteString (encodeByName hdr [x]) output
