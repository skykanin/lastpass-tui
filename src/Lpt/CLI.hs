module CLI
  ( endSession
  , startSession
  , getItems
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
import           Parse                          ( Item
                                                , parseIds
                                                , parseItem
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
startSession :: IO ExitCode
startSession = do
  status <- checkLoginStatus
  if status
    then return ExitSuccess
    else do
      setEnv "LPASS_AGENT_TIMEOUT" "0" -- stop agent from timing out session
      user <- getUser
      loginUser user

endSession :: IO ()
endSession = callProcess "lpass" ["logout", "--force"]

showItems :: IO String
showItems = readProcess "lpass" ["ls"] ""

getJsonItems :: IO [String]
getJsonItems = idList >>= traverse getJsonItem
 where
  getJsonItem iden = readProcess "lpass" ["show", "--json", iden] ""
  idList = parseIds <$> showItems

getItems :: IO [Either String Item]
getItems = map parseItem <$> getJsonItems
