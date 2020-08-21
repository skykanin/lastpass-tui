{- |
   Module      : Main
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

Main entry point for the executable
-}
module Main where

import           CLI                            ( User(..)
                                                , startSession
                                                , getItems
                                                , endSession
                                                , getUser
                                                )
import           System.Exit                    ( ExitCode(..) )
main :: IO ()
main = do
  user <- getUser
  exit <- startSession user
  case exit of
    ExitFailure _ -> putStrLn "Failed to login"
    ExitSuccess   -> do
      items <- getItems (_passwd user)
      print items
      endSession
