{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
   Module      : Parse.Types
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Provides data declarations and JSON decoders for vault item types
-}
module Parse.Types (
    Complex (..),
    Login (..),
    Note (..),
    Item (..),
    getId,
    getGroup,
    getName,
    setGroup,
    setId,
    underscoreParser,
) where

import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict (
    HashMap,
    fromList,
 )
import GHC.Generics

data Login = Login
    { _id :: String
    , _name :: String
    , _username :: String
    , _password :: String
    , _group :: String
    , _url :: String
    , _note :: String
    }
    deriving (Eq, Generic, Show)

data Note = Note
    { _id :: String
    , _name :: String
    , _group :: String
    , _note :: String
    }
    deriving (Eq, Generic, Show)

{- | Represents any of the other item types which store
 | their data in the note field
-}
data Complex = Complex
    { _id :: String
    , _name :: String
    , _group :: String
    , _note :: HashMap String String
    }
    deriving (Eq, Generic, Show)

-- | Represents all the different types in the vault
data Item
    = MkLogin Login
    | MkNote Note
    | MkComplex Complex
    deriving (Eq, Show)

instance FromJSON Login where
    parseJSON = underscoreParser

instance FromJSON Note where
    parseJSON = underscoreParser

instance FromJSON Complex where
    parseJSON = complexParser

-- | Matches json keys with their respective underscore prefixed data type fields
underscoreParser :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
underscoreParser = genericParseJSON underscoreOptions

underscoreOptions :: Options
underscoreOptions = defaultOptions{fieldLabelModifier = stripUnderscore}

stripUnderscore :: String -> String
stripUnderscore ('_' : rest) = rest
stripUnderscore str = str

-- | Parses a Complex type by parsing the value of key 'note' into a hashmap
complexParser :: Value -> Parser Complex
complexParser = withObject "Card" $
    \o -> Complex <$> o .: "id" <*> o .: "name" <*> o .: "group" <*> parseMap o
  where
    parseMap o = parseHashMap <$> o .: "note"

parseHashMap :: String -> HashMap String String
parseHashMap = fromList . map splitOn . lines

splitOn :: String -> (String, String)
splitOn str = (takeWhile notColon str, tail $ dropWhile notColon str)
  where
    notColon = not . (== ':')

getId :: Item -> String
getId (MkLogin login) = _id (login :: Login)
getId (MkComplex complex) = _id (complex :: Complex)
getId (MkNote note) = _id (note :: Note)

setId :: String -> Item -> Item
setId s (MkLogin login) = MkLogin login{_id = s}
setId s (MkComplex complex) = MkComplex complex{_id = s}
setId s (MkNote note) = MkNote note{_id = s}

getGroup :: Item -> String
getGroup (MkLogin login) = _group (login :: Login)
getGroup (MkComplex complex) = _group (complex :: Complex)
getGroup (MkNote note) = _group (note :: Note)

setGroup :: String -> Item -> Item
setGroup s (MkLogin login) = MkLogin login{_group = s}
setGroup s (MkComplex complex) = MkComplex complex{_group = s}
setGroup s (MkNote note) = MkNote note{_group = s}

getName :: Item -> String
getName (MkLogin login) = _name (login :: Login)
getName (MkComplex complex) = _name (complex :: Complex)
getName (MkNote note) = _name (note :: Note)
