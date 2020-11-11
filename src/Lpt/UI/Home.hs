{-# LANGUAGE RecordWildCards #-}
{- |
   Module      : UI.Home
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

UI module dealing with rendering and event handling of the home page
-}
module UI.Home
  ( buildItemInfoRep
  , drawHomepage
  , getItemField
  )
where

import           Brick.Types                    ( Padding(..)
                                                , Widget
                                                )
import           Brick.Widgets.Border           ( borderWithLabel )
import           Brick.Widgets.Core             ( (<+>)
                                                , hLimitPercent
                                                , padAll
                                                , padBottom
                                                , padRight
                                                , str
                                                , strWrap
                                                , vBox
                                                , withAttr
                                                )
import           Brick.Widgets.List             ( listSelectedElement
                                                , renderList
                                                )
import           Data.Bifunctor                 ( bimap )
import           Data.Function                  ( on )
import           Data.HashMap.Strict            ( elems
                                                , keys
                                                )
import           Data.Maybe                     ( fromJust )
import qualified Data.Vector                   as V
import           Parse.Types                    ( Complex(..)
                                                , Login(..)
                                                , Item(..)
                                                , Note(..)
                                                , getName
                                                )
import           UI.Types                       ( FocusedField
                                                , ItemListFocus
                                                , ItemInfo
                                                , ItemList
                                                , Name(..)
                                                )

drawHomepage
  :: ItemList -> ItemListFocus -> ItemInfo -> FocusedField -> [Widget Name]
drawHomepage items listFocus itemInfo focusedField = pure
  (listWidget <+> itemWidget)
 where
  listWidget =
    borderWithLabel (str "Items")
      . hLimitPercent 30
      . renderList (const renderListElement) listFocus
      $ items
  itemWidget = borderWithLabel
    (label selected)
    (renderItemWidget itemInfo listFocus focusedField)
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
renderItemWidget :: ItemInfo -> ItemListFocus -> FocusedField -> Widget Name
renderItemWidget itemInfo listFocus focusedField =
  padAll 1 (renderItemInfo itemInfo listFocus focusedField)

-- | Renders an ItemInfo representation type into a UI widget
renderItemInfo :: ItemInfo -> ItemListFocus -> FocusedField -> Widget Name
renderItemInfo itemInfo listFocus field = itemKeys ks <+> if listFocus
  then itemValsWidget
  else maybe itemValsWidget
             (\i -> vBox $ update i (withAttr "focused") (map strWrap vs))
             focusIndex
 where
  (ks, vs)       = unbuildInfo (hidePassword itemInfo)
  focusIndex     = V.findIndex (\(k, _) -> k == field) itemInfo
  itemValsWidget = itemVals vs
  update _ _ [] = []
  update i f (x : xs) | i == 0    = f x : xs
                      | otherwise = x : update (i - 1) f xs

-- | Hide the password field when rendering ItemInfo
hidePassword :: ItemInfo -> ItemInfo
hidePassword = fmap apply
 where
  apply ("Password", val) = ("Password", obfuscate val)
  apply t                 = t
  obfuscate = map (const '*')

-- | Build an item information rendering representation
buildItemInfoRep :: Item -> ItemInfo
buildItemInfoRep (MkLogin (Login {..})) = buildInfo loginKeys vals
  where vals = [_id, _name, _username, _password, _group, _url, _note]
buildItemInfoRep (MkNote (Note {..})) = buildInfo noteKeys vals
  where vals = [_id, _name, _group, _note]
buildItemInfoRep (MkComplex (Complex {..})) = buildInfo
  (complexKeys ++ keys _note)
  (vals ++ elems _note)
  where vals = [_id, _name, _group]

buildInfo :: [String] -> [String] -> ItemInfo
buildInfo = V.zip `on` V.fromList

unbuildInfo :: ItemInfo -> ([String], [String])
unbuildInfo = bimap V.toList V.toList . V.unzip

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

-- | Retrieve corresponding item info value by key
getItemField :: FocusedField -> ItemInfo -> String
getItemField searchKey =
  snd . fromJust . V.find (\(key, _) -> key == searchKey)
