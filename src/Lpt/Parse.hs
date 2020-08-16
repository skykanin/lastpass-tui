{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuasiQuotes           #-}
module Parse
  ( Item
  , parseIds
  , parseItem
  )
where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.HashMap.Strict            ( HashMap
                                                , fromList
                                                , (!)
                                                )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Text                     as T
import           GHC.Generics
import           Text.RE.TDFA.String

data Login = Login
  { _id :: String
  , _name :: String
  , _fullname :: String
  , _username :: String
  , _password :: String
  , _group :: String
  , _url :: String
  , _note :: String
  } deriving (Generic, Show)

stripUnderscore :: String -> String
stripUnderscore ('_' : rest) = rest
stripUnderscore str          = str

underscoreOptions :: Options
underscoreOptions = defaultOptions { fieldLabelModifier = stripUnderscore }

-- | Matches json keys with their respective underscore prefixed data type fields
underscoreParser :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
underscoreParser = genericParseJSON underscoreOptions

instance FromJSON Login where
  parseJSON = underscoreParser

parseAccount :: String -> Either String [Login]
parseAccount str = eitherDecode (B.pack str)

data Note = Note
  { _id :: String
  , _name :: String
  , _group :: String
  , _note :: String
  } deriving (Generic, Show)

instance FromJSON Note where
  parseJSON = underscoreParser

-- | Represents any of the other item types which store
-- | their data in the note field
data Complex = Complex
  { _id :: String
  , _name :: String
  , _group :: String
  , _note :: HashMap String String
  } deriving (Generic, Show)

instance FromJSON Complex where
  parseJSON = withObject "Card" $ \o ->
    Complex <$> o .: "id" <*> o .: "name" <*> o .: "group" <*> parseMap o
    where parseMap o = parseHashMap <$> o .: "note"

splitOn :: String -> (String, String)
splitOn str = (takeWhile notColon str, tail $ dropWhile notColon str)
  where notColon = not . (== ':')

parseHashMap :: String -> HashMap String String
parseHashMap = fromList . map splitOn . lines

data Item =
    MkLogin Login
  | MkNote Note
  | MkComplex Complex
  deriving (Generic, Show)

instance FromJSON Item

type RawItem = HashMap T.Text T.Text

-- | Parse a JSON string into a hashmap of strings
parseToMap :: String -> Either String (String, RawItem)
parseToMap str = toTuple . fmap unwrap . head <$> obj
 where
  unwrap  = \(String s) -> s
  toTuple = \a -> (str, a)
  obj     = eitherDecode (B.pack str)

-- | Parse JSON string to Item type depending on properties of the
-- | key, value pairs in the hashmap representation of the JSON string
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
