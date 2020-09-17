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

import           UI.Login                       ( tui )

main :: IO ()
main = tui
