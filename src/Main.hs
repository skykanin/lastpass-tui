{-# LANGUAGE DuplicateRecordFields #-}
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
                                                , addItem
                                                , getItems
                                                , endSession
                                                , getUser
                                                )
import           Parse.Types
import           System.Exit                    ( ExitCode(..) )
main :: IO ()
main = do
  user <- getUser
  exit <- startSession user
  case exit of
    ExitFailure _ -> putStrLn "Failed to login"
    ExitSuccess   -> do
      _     <- addItem myItem
      items <- getItems (_passwd user)
      print items
      endSession
 where
  myItem = MkLogin
    (Login { _id       = ""
           , _name     = "xXmemerboi420Xx"
           , _username = "BBBBBOIIIIIIIIIII"
           , _password = "pogupogu2"
           , _group    = ""
           , _url      = "https://memehub.com"
           , _note     = "random meme account"
           }
    )
