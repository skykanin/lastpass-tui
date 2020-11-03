{- |
   Module      : UI.Home
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

UI module dealing with rendering and event handling of the home page
-}
module UI.Home
  ( buildHomepage
  , drawHomepage
  , handleHomepage
  )
where

import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Border           ( borderWithLabel )
import           Brick.Widgets.Core             ( (<+>)
                                                , hLimitPercent
                                                , padRight
                                                , padTopBottom
                                                , str
                                                , strWrap
                                                , vBox
                                                )
import           Brick.Widgets.List             ( List
                                                , list
                                                , listSelectedElement
                                                , handleListEvent
                                                , renderList
                                                )
import           Data.HashMap.Strict            ( elems
                                                , keys
                                                )
import           Data.Vector                    ( fromList )
import           Graphics.Vty.Input.Events      ( Event )
import qualified Parse.Types                   as T
                                                ( Complex(..)
                                                , Login(..)
                                                , Note(..)
                                                )
import           Parse.Types                    ( Item(..)
                                                , getName
                                                )
import           UI.Types                       ( Name(..)
                                                , TuiState(..)
                                                )

drawHomepage :: List Name Item -> [Widget Name]
drawHomepage items = pure (listWidget items <+> itemWidget items)
 where
  listWidget =
    borderWithLabel (str "Items")
      . hLimitPercent 30
      . renderList renderElement True
  itemWidget vaultItems =
    borderWithLabel (label selected) . renderItemWidget $ selected
   where
    label = str . maybe "" (itemType . snd)
    itemType (MkLogin   _) = "Password"
    itemType (MkNote    _) = "Secure Note"
    itemType (MkComplex _) = "Other"
    selected = listSelectedElement vaultItems

renderElement :: Bool -> Item -> Widget Name
renderElement _ item =
  padTopBottom 1 . vBox . map (\f -> str (f item)) $ [getName]

renderItemWidget :: Maybe (Int, Item) -> Widget Name
renderItemWidget (Just (_, item)) = renderItem item
renderItemWidget Nothing          = str "No item selected"

renderItem :: Item -> Widget Name
renderItem (MkLogin (T.Login iden name username password group url note)) =
  infoWidget loginKeys
    <+> vBox (map strWrap [iden, name, username, password, group, url, note])
renderItem (MkNote (T.Note iden name group note)) =
  infoWidget noteKeys <+> vBox (map strWrap [iden, name, group, note])
renderItem (MkComplex (T.Complex iden name group note)) =
  infoWidget (complexKeys ++ keys note)
    <+> vBox (map strWrap $ [iden, name, group] ++ elems note)

infoWidget :: [String] -> Widget Name
infoWidget = padRight (Pad 2) . vBox . map str

loginKeys :: [String]
loginKeys = ["Id", "Name", "Username", "Password", "Group", "Url", "Note"]

noteKeys :: [String]
noteKeys = ["Id", "Name", "Group", "Note"]

complexKeys :: [String]
complexKeys = ["Id", "Name", "Group"]

buildHomepage :: [Item] -> Event -> EventM Name (Next TuiState)
buildHomepage items vtye = handleHomepage itemList vtye
  where itemList = list HomeList (fromList items) 5

handleHomepage :: List Name Item -> Event -> EventM Name (Next TuiState)
handleHomepage items vtye = handleListEvent vtye items >>= continue . Home
