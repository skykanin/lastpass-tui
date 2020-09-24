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
import           CLI                            ( User )
import           Graphics.Vty.Input.Events      ( Event )

data TuiState =
    Login (Form User Event Name, String)
  | Home String

data Name =
    EmailField
  | PasswdField
  | ShowField
  | HomeBox
  deriving (Eq, Ord, Show)
