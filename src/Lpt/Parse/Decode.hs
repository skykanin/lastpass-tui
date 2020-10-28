{-# LANGUAGE QuasiQuotes      #-}
{- |
   Module      : Parse.Decode
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

Provides Item Id string and Item JSON type parsers
-}
module Parse.Decode
  ( parseIds
  , parseItem
  )
where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.HashMap.Strict            ( HashMap
                                                , (!)
                                                )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Text                     as T
import           Parse.Types                    ( Item(..)
                                                , getGroup
                                                , setGroup
                                                )
import           Text.RE.TDFA.String

type RawItem = HashMap T.Text T.Text

-- | Parse a JSON string into a hashmap of strings
parseToMap :: String -> Either String (String, RawItem)
parseToMap str = toTuple . fmap unwrap . head <$> obj
 where
  unwrap  = \(String s) -> s
  toTuple = \a -> (str, a)
  obj     = eitherDecode (B.pack str)

-- | Parse JSON string to Item type depending on properties of the
-- key, value pairs in the hashmap representation of the JSON string
mapToItem :: String -> RawItem -> Either String Item
mapToItem str rawItem
  | T.null usr && note == "NoteType" = MkComplex <$> decoder str
  | not $ T.null usr                 = MkLogin <$> decoder str
  | otherwise                        = MkNote <$> decoder str
 where
  usr  = rawItem ! "username"
  note = T.take 8 $ rawItem ! "note"
  decoder string = head <$> eitherDecode (B.pack string)

patchGroupField :: Item -> Item
patchGroupField item | null $ getGroup item = setGroup "none" item
                     | otherwise            = item

-- | Parse json string to Item
parseItem :: String -> Either String Item
parseItem str = do
  (strKey, rawItem) <- parseToMap str
  item              <- mapToItem strKey rawItem
  pure $ patchGroupField item

-- | Parse out item ids from string output of 'lpass ls'
parseIds :: String -> [String]
parseIds =
  map (drop 4) . mapMaybe (matchedText . (?=~ [re|id: [0-9]+|])) . lines
