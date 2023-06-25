module TextBox where

import Prelude

import Data.Array (head, sort, take, (:))
import Data.Fuzzy (FuzzyStr(..), matchStr)
import Data.Generic.Rep (class Generic)
import Data.Lens (over, set)
import Data.Lens.Prism (Prism', prism', review)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (null)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Halogen (get)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement (focus)
import Web.UIEvent.KeyboardEvent as KeyboardEvent


data Input 
  = ToSearch (Array String)
  | NotToSearch String

data State 
  = NotSelected { search :: String, keywords :: Array String }
  | Selected String
instance Show State where
  show (NotSelected x) = "NotSelected " <> show x
  show (Selected x) = "Selected " <> x
data Output 
  = SelectOutput String
  | Unselect
  | Clicked String

initialState :: Input -> State
initialState (ToSearch keywords) = NotSelected { search: "", keywords }
initialState (NotToSearch kwd) = Selected kwd

data TextBoxQuery a
  = SwitchToEditable (Array String) a

data Action
  = UpdateSearch String
  | SelectKeyword String
  | Unsearch
  | TextBoxClick String
  | GetFocus
  | NoOp
derive instance aaa :: Generic Action _
instance Show Action where
  show = genericShow

_NotSelected :: Prism' State { search :: String, keywords :: Array String }
_NotSelected = prism'
  (\x -> NotSelected x)
  (\s -> case s of
    NotSelected x -> Just x
    _ -> Nothing
  )

_Selected :: Prism' State String
_Selected = prism'
  (\x -> Selected x)
  (\s -> case s of
    Selected x -> Just x
    _ -> Nothing
  )

-- A function that uses the prism to change the state to Selected
-- when the state is NotSelected, and does nothing otherwise.
selectKeyword :: String -> State -> State
selectKeyword str state = case state of
  NotSelected _ -> review _Selected str
  _ -> state


updateSearch :: String -> State -> State
updateSearch newSearch = over _NotSelected (_ { search = newSearch })


component :: forall query m. MonadEffect m => H.Component TextBoxQuery Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
        { handleAction = handleAction
        , handleQuery = handleQuery 
        , initialize = Just GetFocus }
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render = case _ of
    Selected str -> HH.div [HE.onClick \_ -> TextBoxClick str] [ HH.text str ]
    NotSelected { search, keywords } -> renderInput
      where matched = fuzzyMatch search keywords
            renderInput = HH.div_ $
              [ HH.input
                  [ HP.ref (H.RefLabel "input")
                  , HE.onValueInput UpdateSearch
                  , HE.onKeyDown \event ->
                    case Tuple (KeyboardEvent.key event) (Tuple (head matched) search) of
                      Tuple "Enter" (Tuple (Just h) _) -> SelectKeyword h
                      Tuple "Backspace" (Tuple _ "") -> Unsearch
                      _ -> NoOp
                  , HP.value search
                   ]
              ]
              <> 
              if not (null search)
                then [ HH.ul_ <<< map (HH.li_ <<< pure <<< HH.text) $ matched]
                else []
  handleQuery :: forall a. TextBoxQuery a -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    SwitchToEditable keywords a -> do
      H.modify_ case _ of
        Selected sel -> NotSelected {keywords, search: sel}
        x -> x
      H.getHTMLElementRef (H.RefLabel "input") >>= case _ of
        Just x -> liftEffect $ focus x
        Nothing -> pure unit
      pure (Just a)
  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction act = do
    case act of
      UpdateSearch str -> do
        H.modify_ (updateSearch str)
      SelectKeyword str -> do
        H.modify_ (selectKeyword str)
        H.raise $ SelectOutput str
      Unsearch -> H.raise Unselect
      TextBoxClick str -> do
        liftEffect $ log str
        H.raise (Clicked str)
      NoOp -> pure unit
      GetFocus -> H.getHTMLElementRef (H.RefLabel "input") >>= traverse_ (H.liftEffect <<< focus) 

fuzzyMatch :: String -> Array String -> Array String
fuzzyMatch pattern = take 5 <<< map (\(FuzzyStr {original}) -> original) <<< sort <<< map (matchStr false pattern)
