{- |
   Module      : Parse.Encode
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

Provides encoding functions the Item type
-}
module Parse.Encode
  ( fromItem
  , write
  )
where

import           Data.HashMap.Strict            ( HashMap
                                                , toList
                                                )
import           Data.List                      ( intercalate )
import           Parse.Types

write :: Item -> String
write = writeFormat . fromItem

-- | Return writable string
writeFormat :: [(String, String)] -> String
writeFormat = intercalate "\n" . map format
 where
  format ("" , val) = val
  format (key, val) = key ++ ":" ++ val

-- | Convert item types into a list of key value pairs
fromItem :: Item -> [(String, String)]
fromItem (MkLogin (Login _ name username password _ url note)) =
  [ ("Name"    , name)
  , ("Username", username)
  , ("Password", password)
  , ("URL"     , url)
  , ("Notes"   , note)
  ]
fromItem (MkNote (Note _ name _ note)) = [("Name", name), ("Notes", note)]
fromItem (MkComplex (Complex _ name _ note)) =
  [("Name", name), ("", fromMap note)]

-- | Convert complex hashmap value to writable string format
fromMap :: HashMap String String -> String
fromMap = concatMap format . toList
  where format (key, val) = key ++ ":" ++ val ++ "\n"
