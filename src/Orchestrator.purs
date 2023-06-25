module Orchestrator where

import Data.List.NonEmpty
import Prelude

import CSS (color, display, flex, flexDirection, rgb, row)
import Control.Monad.State (class MonadState, get)
import Data.Array (reverse)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List (List(..), foldMap, (..), (:), length)
import Data.Map (Map, fromFoldable, keys, lookup)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import TextBox as TextBox
import Type.Proxy (Proxy(..))

error :: ∀ a. String -> a
error = unsafePerformEffect <<< throw

data Graph
 = Leaf String
 | SubGraph (Map String Graph)
derive instance gaasdgg :: Eq Graph

instance Show Graph where
  show (Leaf s) = "Leaf " <> s
  show (SubGraph g) = "SubGraph " <> show g <> "\n"

type Crumb = {key :: String, above :: Graph}

crumbToStr :: Crumb -> String
crumbToStr c = c.key 



type Ctx = { current :: Graph, crumbs :: List Crumb }

goDown :: String -> Ctx -> Ctx
goDown _ {current: Leaf str, crumbs} = {current: Leaf str, crumbs}
goDown s {current: SubGraph g, crumbs} = case lookup s g of
  Just x -> {current: x, crumbs: {key: s, above: SubGraph g} : crumbs}
  Nothing -> {current: SubGraph g, crumbs} -- doesn't move
goDownS :: String -> State -> State
goDownS str (State {ctx}) = State {ctx: goDown str ctx}

goUp :: Ctx -> Ctx
goUp {crumbs: (Cons ({above: g}) crumbsTail)} = {current: g, crumbs: crumbsTail}
goUp x = x
goUpS :: State -> State
goUpS (State {ctx}) = State {ctx: goUp ctx}

exampleGraph :: Graph
exampleGraph = SubGraph $ 
  fromFoldable 
    [ Tuple "mind" (
      SubGraph $ 
        fromFoldable [
          Tuple "applied-ct" (
            SubGraph $
              fromFoldable [
                  Tuple "spivak" (
                    SubGraph $
                      fromFoldable [
                        Tuple "good" (Leaf "ah")
                      ]
                    )
                , Tuple "fong" (
                  SubGraph $
                    fromFoldable [
                      Tuple "ahahavery" (Leaf "ah")
                    ]
                  )
              ] 
            )
        ]
    )
    , Tuple "body" (
      SubGraph $
        fromFoldable [
          Tuple "baji" (
            SubGraph $
              fromFoldable [
                  Tuple "dabaji" (
                    SubGraph $
                      fromFoldable [
                        Tuple "oh" (Leaf "ah")
                      ]
                    )
                , Tuple "zyq" (
                  SubGraph $
                    fromFoldable [
                      Tuple "oh yea" (Leaf "ah")
                    ]
                  )
              ] 
            )
        ]
      )
    , Tuple "soul" (
      SubGraph $
        fromFoldable [
          Tuple "guitar" (
            SubGraph $
              fromFoldable [
                  Tuple "rock" (
                    SubGraph $
                      fromFoldable [
                        Tuple "ee" (Leaf "ah")
                      ]
                    )
                , Tuple "metal" (
                  SubGraph $
                    fromFoldable [
                      Tuple "laskera" (Leaf "ah")
                    ]
                  )
              ] 
            )
        ]
      )
    ]


newtype State = State { ctx :: Ctx } 
derive instance barselGen :: Generic State _
derive instance gaasd :: Eq State
instance Show State where
  show = genericShow


data Action 
  = Selected String
  | DeleteBox
  | MakeEditable String
derive instance barselActionGen :: Generic Action _
instance Show Action where
  show = genericShow

_search = Proxy :: Proxy "search"
type Slots = ( search :: H.Slot TextBox.TextBoxQuery TextBox.Output Int )

whileM :: forall m s a. MonadState s m => m a -> (s -> Boolean) -> m Unit
whileM action shouldStop = do
  currentState <- get
  if shouldStop currentState then
    pure unit else
    action *> whileM action shouldStop

component ∷ ∀ query input output m. MonadEffect m ⇒ H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval {handleAction = handleAction}
  }
  where
    initialState :: input -> State
    initialState _ = State {ctx: {current: exampleGraph, crumbs: Nil}}
    makeCurrentEditable mod = do
      mod
      State {ctx: {current, crumbs}} <- H.get
      let toTell = length crumbs
      case current of
        SubGraph g -> H.tell _search toTell (TextBox.SwitchToEditable $ Array.fromFoldable $ keys g)
        Leaf str -> error "fodeu"
    handleAction :: Action -> HalogenM State Action Slots output m Unit
    handleAction action = do
      case action of 
        Selected select -> H.modify_ (goDownS select)
        DeleteBox -> makeCurrentEditable (H.modify_ goUpS)
        MakeEditable str -> makeCurrentEditable (whileM (H.modify goUpS) (shouldStop str) *> H.modify_ goUpS)
          where shouldStop str (State{ctx}) = case ctx.crumbs of
                  (Cons {key} _) -> key == str
                  _ -> true
      State {ctx: {crumbs}} <- get
      liftEffect $ logShow (map (\x -> x.key) crumbs)
    render :: ∀ mon. MonadEffect mon => State -> H.ComponentHTML Action Slots mon
    render (State {ctx: {current: j, crumbs}}) =
        case j of
          SubGraph g -> HH.div [] $ 
              foldMap (renderComponent <<< TextBox.ToSearch <<< Array.fromFoldable $ keys g ) (0..length crumbs)
          Leaf leafStr -> 
            HH.div [ ] $
              foldMap (renderComponent $ TextBox.NotToSearch leafStr) (0..length crumbs)
      where cl = HCSS.style do 
                color (rgb 100 0 0)
                display flex
                flexDirection row
            renderComponent :: TextBox.Input -> Int -> Array (H.ComponentHTML Action Slots mon)
            renderComponent g i = [ HH.slot _search i TextBox.component g handleSearchClick ]

            handleSearchClick :: TextBox.Output -> Action
            handleSearchClick (TextBox.SelectOutput outputSelection) = Selected outputSelection
            handleSearchClick TextBox.Unselect = DeleteBox
            handleSearchClick (TextBox.Clicked str) = MakeEditable str
