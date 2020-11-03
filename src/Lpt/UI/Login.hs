{- |
   Module      : UI.Login
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

UI module dealing with the login page rendering and event handling
-}
module UI.Login
  ( drawLoginPage
  , handleLogin
  , loginForm
  , wrap
  )
where

import           Brick.Focus
import           Brick.Forms
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Core
import           CLI                            ( User(..)
                                                , email
                                                , getItems
                                                , passwd
                                                , startSession
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Either                    ( rights )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( unpack )
import           Graphics.Vty.Input.Events      ( Event )
import           System.Exit                    ( ExitCode(..) )
import           UI.Home                        ( buildHomepage )
import           UI.Types                       ( Name(..)
                                                , TuiState(..)
                                                )

drawLoginPage :: Form User e Name -> String -> [Widget Name]
drawLoginPage form [] = fix . renderForm $ form
drawLoginPage form errStr =
  fix . vBox $ [renderForm form, hCenter $ str errStr]

fix :: Widget Name -> [Widget Name]
fix = pure . center . setAvailableSize (50, 10) . borderWithLabel (str "Login")

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

handleLogin :: Form User Event Name -> Event -> EventM Name (Next TuiState)
handleLogin form vtye = if not $ field `elem` [EmailField, PasswdField]
  then continue . wrap $ form
  else do
    form'    <- handleFormEvent (VtyEvent vtye) form
    exitcode <- liftIO $ startSession (formState form')
    case exitcode of
      ExitSuccess -> do
        items <- liftIO . getItems . unpack . _passwd . formState $ form
        buildHomepage (rights items) vtye
      ExitFailure _ -> continue (Login (form', "Wrong username or password"))
  where field = fromJust . focusGetCurrent . formFocus $ form

wrap :: Form User Event Name -> TuiState
wrap form = Login (form, "")
