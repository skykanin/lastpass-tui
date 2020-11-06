{- |
   Module      : UI.Types
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

UI module defining core types
-}

module UI.Types
  ( Name(..)
  , TuiState(..)
  )
where

import           Brick.Forms
import           Brick.Widgets.List             ( List )
import           CLI                            ( User )
import           Graphics.Vty.Input.Events      ( Event )
import           Parse.Types                    ( Item )

type LoginForm = Form User Event Name
type Error = String

type ItemList = List Name Item

data TuiState =
    LoginPage (LoginForm, Error)
  | HomePage ItemList

data Name =
    EmailField
  | PasswdField
  | ShowField
  | HomeList
  deriving (Eq, Ord, Show)
