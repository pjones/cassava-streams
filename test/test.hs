{-# LANGUAGE OverloadedStrings #-}

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
module Main (main) where

--------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import Data.Csv hiding (Record, NamedRecord, record)
import qualified Data.Vector as V
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Csv as CSV
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.Tasty
import Test.Tasty.QuickCheck as QC

--------------------------------------------------------------------------------
-- | Fake record to encode and decode.  This works well because
-- Cassava and QuickCheck already have the necessary instances for
-- triples.
type Record = (Int, String, String)

--------------------------------------------------------------------------------
-- | But, Cassava doesn't have ToNamedRecord, FromNamedRecord
-- instances for triples so we have to work around there here.
newtype NamedRecord = NamedRecord {record :: Record}

instance ToNamedRecord NamedRecord where
  toNamedRecord (NamedRecord (a, b, c)) =
    namedRecord ["a" .= a, "b" .= b, "c" .= c]

instance FromNamedRecord NamedRecord where
  parseNamedRecord m = do
    a <- m .: "a"
    b <- m .: "b"
    c <- m .: "c"
    return $ NamedRecord (a, b, c)


--------------------------------------------------------------------------------
header :: Header
header = V.fromList ["a", "b", "c"]

--------------------------------------------------------------------------------
roundTrip :: [Record] -> IO [Record]
roundTrip recs = do
  -- Encode to ByteString.
  encSource <- Streams.fromList (map NamedRecord recs)
  (collector, flush) <- Streams.listOutputStream
  encoder <- CSV.encodeStreamByName header collector
  Streams.connect encSource encoder

  -- Decode from ByteString.
  decoder <- fmap BS.concat flush      >>=
             Streams.fromByteString    >>=
             CSV.decodeStreamByName    >>=
             CSV.onlyValidRecords

  (outRecs, flush') <- Streams.listOutputStream
  Streams.connect decoder outRecs
  fmap (map record) flush'

--------------------------------------------------------------------------------
prop_namedRoundTrip :: [Record] -> Property
prop_namedRoundTrip recsIn = not (null recsIn) ==> monadicIO $ do
  recsOut <- run $ roundTrip recsIn
  assert $ recsIn == recsOut

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Tests" [ QC.testProperty "namedRoundTrip" $ prop_namedRoundTrip
                          ]

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
