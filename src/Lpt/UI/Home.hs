{- |
   Module      : UI.Home
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

UI module dealing with the home page rendering and event handling
-}
module UI.Home
  ( buildHomepage
  , drawHomepage
  , handleHomepage
  )
where

import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Border           ( border )
import           Brick.Widgets.Core             ( hLimit
                                                , str
                                                , vBox
                                                )
import           Brick.Widgets.List             ( List
                                                , list
                                                , handleListEvent
                                                , renderList
                                                )
import           CLI                            ( User(..)
                                                , getItems
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Either                    ( rights )
import           Data.Text                      ( unpack )
import           Data.Vector                    ( fromList )
import           Graphics.Vty.Input.Events      ( Event )
import           Parse.Types                    ( Item
                                                , getId
                                                , getName
                                                )
import           UI.Types                       ( Name(..)
                                                , TuiState(..)
                                                )

drawHomepage :: List Name Item -> [Widget Name]
drawHomepage = pure . border . hLimit 50 . renderList renderElement True

renderElement :: Bool -> Item -> Widget Name
renderElement focus item =
  highlight . vBox . map (\f -> str (f item)) $ [getId, getName]
  where highlight = if focus then border else id

buildHomepage :: User -> Event -> EventM Name (Next TuiState)
buildHomepage (User _ passwd) vtye = do
  items <- liftIO $ getItems (unpack passwd)
  let itemList = list HomeList (fromList (rights items)) 5
  handleHomepage itemList vtye

handleHomepage :: List Name Item -> Event -> EventM Name (Next TuiState)
handleHomepage items vtye = handleListEvent vtye items >>= continue . Home
