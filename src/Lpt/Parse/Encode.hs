module Parse.Encode
  ( write
  )
where

import qualified Data.HashMap.Strict           as H
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
fromMap :: H.HashMap String String -> String
fromMap = concatMap format . H.toList
  where format (key, val) = key ++ ":" ++ val ++ "\n"
