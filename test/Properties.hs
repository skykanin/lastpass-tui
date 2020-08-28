{-# LANGUAGE FlexibleInstances #-}
module Main where


import           CLI                            ( User(..)
                                                , addItem
                                                , deleteItem
                                                , endSession
                                                , getJsonItem
                                                , startSession
                                                )
import           Data.Aeson                     ( eitherDecode )
import           Data.ByteString.Lazy.Char8     ( pack )
import           Data.HashMap.Strict            ( HashMap
                                                , fromList
                                                )
import           Parse.Decode                   ( parseItem )
import           Parse.Types
import           System.Environment             ( getEnv )
import           System.Exit                    ( ExitCode(..) )
import           Test.QuickCheck.Arbitrary
import           Test.Tasty
import           Test.Tasty.QuickCheck

alphaNum :: Gen Char
alphaNum = elements $ ['a' .. 'z'] ++ ['0' .. '9']

genSafeString :: Gen String
genSafeString = listOf alphaNum `suchThat` (\s -> length s >= 6)

instance Arbitrary Login where
  arbitrary =
    Login
      <$> arbitrary
      <*> genSafeString
      <*> arbitrary
      <*> arbitrary
      <*> genSafeString
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Note where
  arbitrary =
    Note <$> arbitrary <*> genSafeString <*> genSafeString <*> arbitrary

keys :: [String]
keys =
  [ "NoteType"
  , "Expiration Date"
  , "Start Date"
  , "Security Code"
  , "Number"
  , "Type"
  , "Name on Card"
  , "Language"
  , "Notes"
  ]


instance Arbitrary (HashMap String String) where
  arbitrary = fromList <$> traverse (\s -> ((,) s) <$> genSafeString) keys

instance Arbitrary Complex where
  arbitrary =
    Complex <$> arbitrary <*> genSafeString <*> genSafeString <*> arbitrary

instance Arbitrary Item where
  arbitrary =
    chooseInt (1, 3)
      >>= (\n -> case n of
            1 -> MkLogin <$> arbitrary
            2 -> MkNote <$> arbitrary
            3 -> MkComplex <$> arbitrary
          )

getUserFromFile :: FilePath -> IO (Either String User)
getUserFromFile filepath = eitherDecode . pack <$> readFile filepath

-- | Checks if the added item is equivalent when retrieved from the vault
addItemTest :: String -> Item -> IO Property
addItemTest passwd item = do
  exitcode <- addItem item
  case exitcode of
    ExitFailure _ -> return $ property False
    ExitSuccess   -> do
      eitherItem <- parseItem <$> getJsonItem passwd (getName item)
      return $ case eitherItem of
        Left _ -> property False
        Right retItem ->
          let updatedItem =
                setId (getId retItem) .
                setGroup (getGroup retItem) $ retItem
          in updatedItem === retItem

-- | Run all tests
main :: IO ()
main = do
  eitherUser <- getUserFromFile "misc/testUser.json"
  case eitherUser of
    Left  err  -> print err
    Right user -> do
      _ <- startSession user
      defaultMain $
        testProperty "setting and then getting item should return same item" $
          withMaxSuccess 50 (ioProperty . (addItemTest $ passwd user))
      endSession
