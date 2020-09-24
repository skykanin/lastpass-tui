{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : UI.Core
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

UI module dealing with core brick logic
-}
module UI.Core
  ( tui
  )
where

import           Brick.AttrMap
import           Brick.Focus
import           Brick.Forms
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Core
import qualified Brick.Widgets.Edit            as E
import           Brick.Util                     ( on )
import           CLI                            ( User(..)
                                                , endSession
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events
import           UI.Login
import           UI.Types                       ( Name(..)
                                                , TuiState(..)
                                                )

tui :: IO ()
tui = do
  initialState <- buildInitialState
  _            <- defaultMain tuiApp initialState
  return ()


tuiApp :: App TuiState e Name
tuiApp = App { appDraw         = drawTui
             , appChooseCursor = focusRingCursor focus
             , appHandleEvent  = handleTuiEvent
             , appStartEvent   = pure
             , appAttrMap      = const theMap
             }

focus :: TuiState -> FocusRing Name
focus (Login (form, _)) = formFocus form
focus (Home  _        ) = focusRing [HomeBox]

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [ (E.editAttr          , V.white `on` V.black)
  , (E.editFocusedAttr   , V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

buildInitialState :: IO TuiState
buildInitialState = pure . Login $ (loginForm (User "" ""), "")

drawTui :: TuiState -> [Widget Name]
drawTui (Login tuple) = uncurry drawLoginPage $ tuple
drawTui (Home string) =
  pure . center . hLimit 50 . borderWithLabel (str "Home") . str $ string

handleTuiEvent :: TuiState -> BrickEvent Name e -> EventM Name (Next TuiState)
handleTuiEvent state event = case state of
  Login (form, _) -> case event of
    VtyEvent vtye -> case vtye of
      EvKey KEsc [] -> exitThenHalt state
      EvKey KEnter [] -> handleLogin form vtye
      _ -> handleFormEvent (VtyEvent vtye) form >>= continue . wrap
    _ -> continue state
  Home _ -> case event of
    VtyEvent vtye -> case vtye of
      EvKey KEsc [] -> exitThenHalt state
      _             -> continue state
    _ -> continue state

exitThenHalt :: TuiState -> EventM Name (Next TuiState)
exitThenHalt state = liftIO endSession >> halt state
