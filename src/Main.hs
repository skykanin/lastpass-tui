{- |
   Module      : Main
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

Main entry point for the executable
-}
module Main where

import           CLI                            ( startSession
                                                , getItems
                                                , endSession
                                                )

main :: IO ()
main = do
  _     <- startSession
  items <- getItems
  print items
  endSession
