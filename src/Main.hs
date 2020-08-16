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
