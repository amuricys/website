module Test.Gen where

import Prelude
import Test.QuickCheck.Arbitrary

import Control.Monad.Gen (oneOf)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, fromFoldable, singleton)
import Data.Array.NonEmpty as NAE
import Data.List (List, fromFoldable)
import Data.List as L
import Data.Map (keys)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Orchestrator (Graph(..), State(..), exampleGraph, goDownS, goUpS)
import Test.QuickCheck (quickCheck, quickCheckGen, (===))
import Test.QuickCheck.Gen (Gen, arrayOf, arrayOf1, choose, elements)

graphKeys :: Graph -> NonEmptyArray String
graphKeys (SubGraph g) = case NAE.fromFoldable $ keys g of
    Just x -> x
    Nothing -> singleton "dummy"
graphKeys (Leaf x) = singleton x

porra :: Effect Unit
porra = quickCheckGen $ do
    pool <- ((graphKeys exampleGraph <> _) <$> arrayOf1 arbitrary)
    toDive <- elements pool
    let baseState = State {ctx: {current: exampleGraph, crumbs: L.fromFoldable []} }
        newState = goDownS toDive baseState
        backState = goUpS newState
    pure $ backState === baseState
