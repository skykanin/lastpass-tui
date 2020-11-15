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
import CLI
  ( User (..),
    checkLoginStatus,
    getItems,
  )
import Data.Either (rights)
import qualified Data.Vector as V
import qualified Graphics.Vty as GV
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
    GV.defAttr
    [ (E.editAttr, GV.white `on` GV.black),
      (E.editFocusedAttr, GV.black `on` GV.yellow),
      (invalidFormInputAttr, GV.white `on` GV.red),
      (focusedFormInputAttr, GV.black `on` GV.yellow),
      (L.listSelectedFocusedAttr, GV.black `on` GV.yellow),
      (attrName "focused", GV.black `on` GV.yellow)
    ]

buildInitialState :: IO TuiState
buildInitialState = do
  loginStatus <- checkLoginStatus
  if loginStatus
    then do
      eitherItems <- getItems ""
      let items = rights eitherItems
          itmList = L.list ItemList (V.fromList items) 5
          itmInfo = buildItemInfoRep (head items)
          focusedField = fst (V.head itmInfo)
      pure $ HomePage itmList True itmInfo focusedField
    else pure $ LoginPage (loginForm (User "" ""), "")

drawTui :: TuiState -> [Widget Name]
drawTui (LoginPage tuple) = uncurry drawLoginPage $ tuple
drawTui (HomePage {..}) =
  drawHomepage itemList listFocus itemInfo focusedItemField
