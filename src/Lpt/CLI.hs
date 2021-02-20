{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
   Module      : CLI
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Provides all the commands wrapping the lastpass-cli
-}
module CLI (
    User (..),
    checkLoginStatus,
    email,
    passwd,
    endSession,
    startSession,
    addItem,
    deleteItem,
    getItems,
    getJsonItem,
    editItem,
    getUser,
) where

import Control.Exception (bracket_)
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics
import Lens.Micro.TH (makeLenses)
import Parse.Decode (
    parseIds,
    parseItem,
 )
import Parse.Encode (write)
import Parse.Types (
    Item,
    getId,
    getName,
    underscoreParser,
 )
import System.Environment (setEnv)
import System.Exit (ExitCode (..))
import System.IO (
    hFlush,
    hGetEcho,
    hSetEcho,
    stdin,
    stdout,
 )
import System.Process (
    callProcess,
    readProcess,
    readProcessWithExitCode,
 )

data User = User
    { _email :: T.Text
    , _passwd :: T.Text
    }
    deriving (Generic, Eq, Show)

makeLenses ''User

instance FromJSON User where
    parseJSON = underscoreParser

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getEmail :: IO T.Text
getEmail = do
    putStr "Email: "
    hFlush stdout
    T.getLine

-- | Get password without terminal echoing
getPassword :: IO T.Text
getPassword = do
    putStr "Password: [Input Hidden]"
    hFlush stdout
    pass <- withEcho False T.getLine
    putChar '\n'
    return pass

getUser :: IO User
getUser = User <$> getEmail <*> getPassword

checkLoginStatus :: IO Bool
checkLoginStatus = do
    (exit, _, _) <- readProcessWithExitCode "lpass" ["status"] ""
    return $ case exit of
        ExitSuccess -> True
        ExitFailure _ -> False

-- | Attempt user login and return appropriate exit code
loginUser :: User -> IO ExitCode
loginUser (User mail pass) = do
    (exit, _, _) <-
        readProcessWithExitCode
            "lpass"
            ["login", "--trust", (T.unpack mail)]
            (T.unpack pass)
    return exit

-- | Start a lastpass session and login user
startSession :: User -> IO ExitCode
startSession user = do
    status <- checkLoginStatus
    if status
        then return ExitSuccess
        else do
            setEnv "LPASS_AGENT_TIMEOUT" "0" -- stop agent from timing out session
            setEnv "LPASS_DISABLE_PINENTRY" "1" -- stop using pinentry for password prompt
            loginUser user

endSession :: IO ()
endSession = callProcess "lpass" ["logout", "--force"]

showItems :: IO String
showItems = readProcess "lpass" ["ls"] ""

getJsonItems :: String -> IO [String]
getJsonItems password = idList >>= traverse (getJsonItem password)
  where
    idList = parseIds <$> showItems

getJsonItem :: String -> String -> IO String
getJsonItem password iden =
    readProcess "lpass" ["show", "--json", iden] password

getItems :: String -> IO [Either String Item]
getItems password = map parseItem <$> getJsonItems password

-- | TODO: maybe return error on fail
editItem :: Item -> IO ()
editItem item = do
    _ <-
        readProcess
            "lpass"
            ["edit", "--non-interactive", (getId item)]
            (write item)
    return ()

-- | If item name is not unique process fails
deleteItem :: String -> IO ExitCode
deleteItem itemId = do
    (exit, _, _) <- readProcessWithExitCode "lpass" ["rm", itemId] ""
    return exit

{- | Write item to the vault using the name field as the entry name.
 id field is ignored, because one is generated when the item is created
-}
addItem :: Item -> IO ExitCode
addItem item = do
    (exit, _, _) <-
        readProcessWithExitCode
            "lpass"
            ["add", "--non-interactive", (getName item)]
            (write item)
    return exit
