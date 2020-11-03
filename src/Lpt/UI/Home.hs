{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications      #-}
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
renderListElement = padTopBottom 1 . str . getName

-- | Renders the item widget with more detailed information
renderItemWidget :: Maybe (Int, Item) -> Widget Name
renderItemWidget (Just (_, item)) = renderItem item
renderItemWidget Nothing          = str "No item selected"

-- | Renders an Item type into a UI widget
renderItem :: Item -> Widget Name
renderItem (MkLogin login) = infoWidget loginKeys
  <+> vBox (map (\f -> strWrap $ f login) listVals)
 where
  listVals :: [Login -> String]
  listVals = [_id, _name, _username, _password, _group, _url, _note]
renderItem (MkNote note) = infoWidget noteKeys
  <+> vBox (map (\f -> strWrap $ f note) listVals)
 where
  listVals :: [Note -> String]
  listVals = [_id, _name, _group, _note]
renderItem (MkComplex complex) = infoWidget (complexKeys ++ keys note)
  <+> vBox (map strWrap $ (map ($ complex) listVals) ++ elems note)
 where
  listVals :: [Complex -> String]
  listVals = [_id, _name, _group]
  note     = _note (complex :: Complex)

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
