{-# LANGUAGE QuasiQuotes      #-}
{- |
   Module      : Parse.Decode
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

Provides Item Id string and Item JSON type parser
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
import           Parse.Types                    ( Item(..) )
import           Text.RE.TDFA.String


-- | Parse a JSON string into a hashmap of strings
parseToMap :: String -> Either String (String, RawItem)
parseToMap str = toTuple . fmap unwrap . head <$> obj
 where
  unwrap  = \(String s) -> s
  toTuple = \a -> (str, a)
  obj     = eitherDecode (B.pack str)

type RawItem = HashMap T.Text T.Text

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

-- | Parse json string to Item
parseItem :: String -> Either String Item
parseItem str = parseToMap str >>= uncurry mapToItem

-- | Parse out item ids from string output of 'lpass ls'
parseIds :: String -> [String]
parseIds =
  map (drop 4) . mapMaybe (matchedText . (?=~ [re|id: [0-9]+|])) . lines
