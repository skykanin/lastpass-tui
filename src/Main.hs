module Main where


import           CLI                            ( startSession )

main :: IO ()
main = startSession >> return ()
