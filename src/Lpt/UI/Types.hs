-- |
--   Module      : UI.Types
--   License     : GNU GPL, version 3 or above
--   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--   Stability   : alpha
--   Portability : portable
--
-- UI module defining core types
module UI.Types
  ( FocusedField,
    ItemListFocus,
    ItemInfo,
    ItemList,
    Name (..),
    TuiState (..),
  )
where

import Brick.Forms
import Brick.Widgets.List (List)
import CLI (User)
import Data.Vector (Vector)
import Graphics.Vty.Input.Events (Event)
import Parse.Types (Item)

type LoginForm = Form User Event Name

type Error = String

type ItemList = List Name Item

type ItemListFocus = Bool

type Key = String

type Val = String

type ItemInfo = Vector (Key, Val)

type FocusedField = String

data TuiState
  = LoginPage (LoginForm, Error)
  | HomePage
      { itemList :: ItemList,
        listFocus :: ItemListFocus,
        itemInfo :: ItemInfo,
        focusedItemField :: FocusedField
      }

data Name
  = EmailField
  | PasswdField
  | ShowField
  | ItemList
  | ItemInfo
  deriving (Eq, Ord, Show)
