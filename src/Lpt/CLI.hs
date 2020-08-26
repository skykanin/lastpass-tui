{- |
   Module      : CLI
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

Provides all the commands wrapping the lastpass-cli
-}
module CLI
  ( User(..)
  , endSession
  , startSession
  , addItem
  , deleteItem
  , getItems
  , editItem
  , getUser
  )
where

import           Control.Exception              ( bracket_ )
import           System.IO                      ( hFlush
                                                , hGetEcho
                                                , hSetEcho
                                                , stdin
                                                , stdout
                                                )
import           System.Environment             ( setEnv )
import           System.Exit                    ( ExitCode(..) )
import           System.Process                 ( callProcess
                                                , readProcessWithExitCode
                                                , readProcess
                                                )
import           Parse.Decode                   ( parseIds
                                                , parseItem
                                                )
import           Parse.Encode                   ( write )
import           Parse.Types                    ( Item
                                                , getId
                                                , getName
                                                )

data User = User
  { _email :: String,
    _passwd :: String
  }
  deriving (Eq, Show)

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getEmail :: IO String
getEmail = do
  putStr "Email: "
  hFlush stdout
  getLine

-- | Get password without terminal echoing
getPassword :: IO String
getPassword = do
  putStr "Password: [Input Hidden]"
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

getUser :: IO User
getUser = User <$> getEmail <*> getPassword

checkLoginStatus :: IO Bool
checkLoginStatus = do
  (exit, _, _) <- readProcessWithExitCode "lpass" ["status"] ""
  return $ case exit of
    ExitSuccess   -> True
    ExitFailure _ -> False

-- | Attempt user login and return appropriate exit code
loginUser :: User -> IO ExitCode
loginUser (User email passwd) = do
  (exit, _, _) <- readProcessWithExitCode "lpass"
                                          ["login", "--trust", email]
                                          passwd
  return exit

-- | Start a lastpass session and login user
startSession :: User -> IO ExitCode
startSession user = do
  status <- checkLoginStatus
  if status
    then return ExitSuccess
    else do
      setEnv "LPASS_AGENT_TIMEOUT"    "0" -- stop agent from timing out session
      setEnv "LPASS_DISABLE_PINENTRY" "1" -- stop using pinentry for password prompt
      loginUser user

endSession :: IO ()
endSession = callProcess "lpass" ["logout", "--force"]

showItems :: IO String
showItems = readProcess "lpass" ["ls"] ""

getJsonItems :: String -> IO [String]
getJsonItems password = idList >>= traverse (getJsonItem password)
 where
  getJsonItem passwd iden = readProcess "lpass" ["show", "--json", iden] passwd
  idList = parseIds <$> showItems

getItems :: String -> IO [Either String Item]
getItems password = map parseItem <$> getJsonItems password

-- | TODO: maybe return error on fail
editItem :: Item -> IO ()
editItem item = do
  _ <- readProcess "lpass"
                   ["edit", "--non-interactive", (getId item)]
                   (write item)
  return ()

-- | If item name is not unique process fails
deleteItem :: String -> IO ExitCode
deleteItem itemId = do
  (exit, _, _) <- readProcessWithExitCode "lpass" ["rm", itemId] ""
  return exit

-- | Write item to the vault using the name field as the entry name.
-- id field is ignored, because one is generated when the item is created
addItem :: Item -> IO ExitCode
addItem item = do
  (exit, _, _) <- readProcessWithExitCode
    "lpass"
    ["add", "--non-interactive", (getName item)]
    (write item)
  return exit
