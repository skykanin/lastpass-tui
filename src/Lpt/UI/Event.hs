-- |
--   Module      : UI.Event
--   License     : GNU GPL, version 3 or above
--   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--   Stability   : alpha
--   Portability : portable
--
-- UI module dealing with brick event handling
module UI.Event
  ( handleTuiEvent,
  )
where

import Brick.Focus (focusGetCurrent)
import Brick.Forms
  ( Form,
    formFocus,
    formState,
    handleFormEvent,
  )
import Brick.Main
  ( continue,
    halt,
  )
import Brick.Types
  ( BrickEvent (..),
    Direction (..),
    EventM,
    Next,
  )
import Brick.Util (clamp)
import Brick.Widgets.List
  ( handleListEvent,
    list,
    listSelectedElement,
  )
import CLI
  ( User (..),
    endSession,
    getItems,
    startSession,
  )
import Control.Monad.IO.Class (liftIO)
import Data.Either (rights)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import qualified Data.Vector as V
import Graphics.Vty.Input.Events
  ( Event (..),
    Key (..),
    Modifier (..),
  )
import Parse.Types (Item)
import System.Exit (ExitCode (..))
import System.Hclip (setClipboard)
import UI.Home
  ( buildItemInfoRep,
    getItemField,
  )
import UI.Types

-- | Main function for handling UI events
handleTuiEvent :: TuiState -> BrickEvent Name e -> EventM Name (Next TuiState)
handleTuiEvent state event = case state of
  LoginPage (form, _) -> case event of
    VtyEvent vtye -> case vtye of
      EvKey KEsc [] -> exitThenHalt state
      EvKey KEnter [] -> handleLogin form vtye
      _ -> handleFormEvent (VtyEvent vtye) form >>= continue . wrap
    _ -> continue state
  HomePage il lf ii fif -> case event of
    VtyEvent vtye -> case vtye of
      EvKey KEsc [] -> exitThenHalt state
      EvKey KRight [] -> handleHomepage vtye il False ii fif
      EvKey KLeft [] -> handleHomepage vtye il True ii fif
      EvKey (KChar 'c') [MCtrl] ->
        if not lf
          then do
            _ <- liftIO $ setClipboard (getItemField fif ii)
            handleHomepage vtye il lf ii fif
          else handleHomepage vtye il lf ii fif
      _ -> handleHomepage vtye il lf ii fif
    _ -> continue state

exitThenHalt :: TuiState -> EventM Name (Next TuiState)
exitThenHalt state = liftIO endSession >> halt state

-- | Handles login form events
handleLogin :: Form User Event Name -> Event -> EventM Name (Next TuiState)
handleLogin form vtye =
  if not $ field `elem` [EmailField, PasswdField]
    then continue . wrap $ form
    else do
      form' <- handleFormEvent (VtyEvent vtye) form
      exitcode <- liftIO $ startSession (formState form')
      case exitcode of
        ExitSuccess -> do
          items <- liftIO . getItems . unpack . _passwd . formState $ form
          buildHomepage vtye (rights items)
        ExitFailure _ ->
          continue (LoginPage (form', "Wrong username or password"))
  where
    field = fromJust . focusGetCurrent . formFocus $ form

wrap :: Form User Event Name -> TuiState
wrap form = LoginPage (form, "")

-- | Build an initial homepage state
buildHomepage :: Event -> [Item] -> EventM Name (Next TuiState)
buildHomepage vtye items =
  handleHomepage
    vtye
    itmList
    True
    itmInfo
    focusedField
  where
    itmList = list ItemList (V.fromList items) 5
    itmInfo = buildItemInfoRep (head items)
    focusedField = fst (V.head itmInfo)

-- | Handles event for the homepage
handleHomepage ::
  Event ->
  ItemList ->
  ItemListFocus ->
  ItemInfo ->
  FocusedField ->
  EventM Name (Next TuiState)
handleHomepage vtye il ilf ii ff =
  if ilf
    then do
      il' <- handleListEvent vtye il
      let itemInfo' =
            buildItemInfoRep . snd . fromJust . listSelectedElement $ il'
      let ff' = fst (V.head itemInfo')
      continue (HomePage il' ilf itemInfo' ff')
    else case vtye of
      EvKey KUp [] ->
        let nff = newFocus ff Up ii in continue (HomePage il ilf ii nff)
      EvKey KDown [] ->
        let nff = newFocus ff Down ii in continue (HomePage il ilf ii nff)
      _ -> continue (HomePage il ilf ii ff)

-- | Return the new focused field given a direction
newFocus :: FocusedField -> Direction -> ItemInfo -> FocusedField
newFocus field direction info = case direction of
  Up -> ks V.! clampLen (fieldIndex - 1)
  Down -> ks V.! clampLen (fieldIndex + 1)
  where
    clampLen = clamp 0 (V.length info - 1)
    fieldIndex = fromJust (V.elemIndex field ks)
    (ks, _) = V.unzip info
