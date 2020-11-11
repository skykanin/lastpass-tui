{-# LANGUAGE RecordWildCards #-}

-- |
--   Module      : UI.Core
--   License     : GNU GPL, version 3 or above
--   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--   Stability   : alpha
--   Portability : portable
--
-- UI module dealing with core brick logic
module UI.Core
  ( tui,
  )
where

import Brick.AttrMap
import Brick.Focus
import Brick.Forms
import Brick.Main
import Brick.Types
import Brick.Util (on)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import CLI (User (..))
import qualified Graphics.Vty as V
import UI.Event (handleTuiEvent)
import UI.Home
import UI.Login
import UI.Types
  ( Name (..),
    TuiState (..),
  )

-- | Application entry point
tui :: IO ()
tui = do
  initialState <- buildInitialState
  _ <- defaultMain tuiApp initialState
  return ()

-- | Top level application definition
tuiApp :: App TuiState e Name
tuiApp =
  App
    { appDraw = drawTui,
      appChooseCursor = focusRingCursor focus,
      appHandleEvent = handleTuiEvent,
      appStartEvent = pure,
      appAttrMap = const theMap
    }

focus :: TuiState -> FocusRing Name
focus (LoginPage (form, _)) = formFocus form
focus (HomePage {..}) = focusRing [item]
  where
    item = if listFocus then ItemList else ItemInfo

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.black),
      (E.editFocusedAttr, V.black `on` V.yellow),
      (invalidFormInputAttr, V.white `on` V.red),
      (focusedFormInputAttr, V.black `on` V.yellow),
      (L.listSelectedFocusedAttr, V.black `on` V.yellow),
      (attrName "focused", V.black `on` V.yellow)
    ]

buildInitialState :: IO TuiState
buildInitialState = pure . LoginPage $ (loginForm (User "" ""), "")

drawTui :: TuiState -> [Widget Name]
drawTui (LoginPage tuple) = uncurry drawLoginPage $ tuple
drawTui (HomePage {..}) =
  drawHomepage itemList listFocus itemInfo focusedItemField
