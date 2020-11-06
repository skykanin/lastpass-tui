{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
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
                                                , padAll
                                                , padBottom
                                                , padRight
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
import           Parse.Types                    ( Complex(..)
                                                , Login(..)
                                                , Item(..)
                                                , Note(..)
                                                , getName
                                                )
import           UI.Types                       ( Name(..)
                                                , TuiState(..)
                                                )

drawHomepage :: List Name Item -> [Widget Name]
drawHomepage items = pure (listWidget <+> itemWidget)
 where
  listWidget =
    borderWithLabel (str "Items")
      . hLimitPercent 30
      . renderList (const renderListElement) True
      $ items
  itemWidget = borderWithLabel (label selected) (renderItemWidget selected)
   where
    label = str . itemType . fmap snd
    itemType (Just (MkLogin   _)) = "Password"
    itemType (Just (MkNote    _)) = "Secure Note"
    itemType (Just (MkComplex _)) = "Other"
    itemType Nothing              = ""
    selected = listSelectedElement items

-- | Renders a single list item element
renderListElement :: Item -> Widget Name
renderListElement = padRight Max . padBottom (Pad 1) . str . getName

-- | Renders the item widget with more detailed information
renderItemWidget :: Maybe (Int, Item) -> Widget Name
renderItemWidget (Just (_, item)) = padAll 1 (renderItem item)
renderItemWidget Nothing          = str "No item selected"

-- | Renders an Item type into a UI widget
renderItem :: Item -> Widget Name
renderItem (MkLogin (Login {..})) = itemKeys loginKeys <+> itemVals vals
  where vals = [_id, _name, _username, _password, _group, _url, _note]
renderItem (MkNote (Note {..})) = itemKeys noteKeys <+> itemVals vals
  where vals = [_id, _name, _group, _note]
renderItem (MkComplex (Complex {..})) = itemKeys (complexKeys ++ keys _note)
  <+> itemVals (vals ++ elems _note)
  where vals = [_id, _name, _group]

itemKeys :: [String] -> Widget Name
itemKeys = padRight (Pad 2) . vBox . map str

itemVals :: [String] -> Widget Name
itemVals = vBox . map strWrap

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
handleHomepage items vtye = handleListEvent vtye items >>= continue . HomePage
