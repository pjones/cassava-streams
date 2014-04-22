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
import System.Environment
import System.IO
import System.IO.Streams.Csv.Tutorial

--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs

  case args of
    ["todo"]    -> onlyTodo stdin stdout
    ["done", x] -> markDone x stdin stdout
    _           -> fail "give one of todo or done"
