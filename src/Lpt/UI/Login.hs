{- |
   Module      : UI.Login
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 UI module dealing with the login page rendering and event handling
-}
module UI.Login (
  drawLoginPage,
  loginForm,
) where

import Brick.Forms
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import CLI (
  User,
  email,
  passwd,
 )
import UI.Types (Name (..))

drawLoginPage :: Form User e Name -> String -> [Widget Name]
drawLoginPage form [] = fix . renderForm $ form
drawLoginPage form errStr =
  fix . vBox $ [renderForm form, hCenter $ str errStr]

fix :: Widget Name -> [Widget Name]
fix = pure . center . setAvailableSize (50, 10) . borderWithLabel (str "Login")

loginForm :: User -> Form User e Name
loginForm =
  let label s w =
        padTop (Pad 1) $
          padBottom (Pad 1) $
            (vLimit 1 $ hLimit 10 $ str s <+> fill ' ')
              <+> w
   in newForm
        [ label "Email:" @@= editTextField email EmailField (Just 1)
        , label "Passwd:" @@= editPasswordField passwd PasswdField
        ]
