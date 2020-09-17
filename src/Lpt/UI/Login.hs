{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : UI.Login
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

UI module dealing with the login page
-}
module UI.Login
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
import           CLI                            ( User(..)
                                                , email
                                                , passwd
                                                )
import           Brick.Util                     ( on )
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState     <- defaultMain tuiApp initialState
  print $ formState endState

tuiApp :: App (Form User e Name) e Name
tuiApp = App { appDraw         = drawTui
             , appChooseCursor = focusRingCursor formFocus
             , appHandleEvent  = handleTuiEvent
             , appStartEvent   = pure
             , appAttrMap      = const theMap
             }

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [ (E.editAttr          , V.white `on` V.black)
  , (E.editFocusedAttr   , V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

buildInitialState :: IO (Form User e Name)
buildInitialState = pure $ loginForm (User "email" "password")

data Name =
    EmailField
  | PasswdField
  deriving (Eq, Ord, Show)

loginForm :: User -> Form User e Name
loginForm =
  let label s w =
          padTop (Pad 1)
            $   padBottom (Pad 1)
            $   (vLimit 1 $ hLimit 10 $ str s <+> fill ' ')
            <+> w
  in  newForm
        [ label "Email:" @@= editTextField email EmailField (Just 1)
        , label "Passwd:" @@= editPasswordField passwd PasswdField
        ]

showForm :: Form User e Name -> Widget Name
showForm = center . hLimit 30 . borderWithLabel (str "Login") . renderForm

drawTui :: Form User e Name -> [Widget Name]
drawTui = pure . showForm

handleTuiEvent
  :: Eq n => Form s e n -> BrickEvent n e -> EventM n (Next (Form s e n))
handleTuiEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> halt s
    _                    -> handleFormEvent e s >>= continue

  _ -> continue s
