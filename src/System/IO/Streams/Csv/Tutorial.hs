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
-- | A simple tutorial on using the cassava-streams library to glue
-- together cassava and io-streams.
--
-- Note: if you're reading this on Hackage or in Haddock then you
-- should switch to source view with the \"Source\" link at the top of
-- this page or open this file in your favorite text editor.
module System.IO.Streams.Csv.Tutorial
       ( -- * Types representing to-do items and their state
         Item (..)
       , TState (..)

         -- * Functions which use cassava-streams functions
       , onlyTodo
       , markDone
       ) where

--------------------------------------------------------------------------------
import Control.Monad
import Data.Csv
import qualified Data.Vector as V
import System.IO
import qualified System.IO.Streams as Streams
import System.IO.Streams.Csv

--------------------------------------------------------------------------------
-- | A to-do item.
data Item = Item
  { title :: String       -- ^ Title.
  , state :: TState       -- ^ State.
  , time  :: Maybe Double -- ^ Seconds taken to complete.
  } deriving (Show, Eq)

instance FromNamedRecord Item where
  parseNamedRecord m = Item <$> m .: "Title"
                            <*> m .: "State"
                            <*> m .: "Time"

instance ToNamedRecord Item where
  toNamedRecord (Item t s tm) =
    namedRecord [ "Title" .= t
                , "State" .= s
                , "Time"  .= tm
                ]

--------------------------------------------------------------------------------
-- | Possible states for a to-do item.
data TState = Todo -- ^ Item needs to be completed.
            | Done -- ^ Item has been finished.
            deriving (Show, Eq)

instance FromField TState where
  parseField "TODO" = return Todo
  parseField "DONE" = return Done
  parseField _      = mzero

instance ToField TState where
  toField Todo = "TODO"
  toField Done = "DONE"

--------------------------------------------------------------------------------
-- | The @onlyTodo@ function reads to-do 'Item's from the given input
-- handle (in CSV format) and writes them back to the output handle
-- (also in CSV format), but only if the items are in the @Todo@
-- state.  In another words, the CSV data is filtered so that the
-- output handle only receives to-do 'Item's which haven't been
-- completed.
--
-- The io-streams @handleToInputStream@ function is used to create an
-- @InputStream ByteString@ stream from the given input handle.
--
-- That stream is then given to the cassava-streams function
-- 'decodeStreamByName' which converts the @InputStream ByteString@
-- stream into an @InputStream Item@ stream.
--
-- Notice that the cassava-streams function 'onlyValidRecords' is used
-- to transform the decoding stream into one that only produces valid
-- records.  Any records which fail type conversion (via
-- @FromNamedRecord@ or @FromRecord@) will not escape from
-- 'onlyValidRecords' but instead will throw an exception.
--
-- Finally the io-streams @filter@ function is used to filter the
-- input stream so that it only produces to-do items which haven't
-- been completed.
onlyTodo :: Handle -- ^ Input handle where CSV data can be read.
         -> Handle -- ^ Output handle where CSV data can be written.
         -> IO ()
onlyTodo inH outH = do
  -- A stream which produces items which are not 'Done'.
  input  <- Streams.handleToInputStream inH         >>=
            decodeStreamByName >>= onlyValidRecords >>=
            Streams.filter (\item -> state item /= Done)

  -- A stream to write items into.  They will be converted to CSV.
  output <- Streams.handleToOutputStream outH >>=
            encodeStreamByName (V.fromList ["State", "Time", "Title"])

  -- Connect the input and output streams.
  Streams.connect input output

--------------------------------------------------------------------------------
-- | The @markDone@ function will read to-do items from the given
-- input handle and mark any matching items as @Done@.  All to-do
-- items are written to the given output handle.
markDone :: String -- ^ Items with this title are marked as @Done@.
         -> Handle -- ^ Input handle where CSV data can be read.
         -> Handle -- ^ Output handle where CSV data can be written.
         -> IO ()
markDone titleOfItem inH outH = do
  -- Change matching items to the 'Done' state.
  let markDone' item = if title item == titleOfItem
                         then item {state = Done}
                         else item

  -- A stream which produces items and converts matching items to the
  -- 'Done' state.
  input  <- Streams.handleToInputStream inH         >>=
            decodeStreamByName >>= onlyValidRecords >>=
            Streams.map markDone'

  -- A stream to write items into.  They will be converted to CSV.
  output <- Streams.handleToOutputStream outH >>=
            encodeStreamByName (V.fromList ["State", "Time", "Title"])

  -- Connect the input and output streams.
  Streams.connect input output
