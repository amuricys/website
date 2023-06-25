module Test.Main where

import Orchestrator
import Prelude
import Test.Gen

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it, beforeAll_)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import TextBox as TextBox

baseState :: State
baseState = State {ctx: {current: exampleGraph, crumbs: Nil}}

testGoDownS :: Spec Unit
testGoDownS = describe "goDownS tests" do
  it "goDownS on baseState with 'mind' should not be baseState" do
    let newState = goDownS "mind" baseState
    newState `shouldNotEqual` baseState

  it "goDownS on baseState with non-existent node should be baseState" do
    let newState = goDownS "nonExistentNode" baseState
    newState `shouldEqual` baseState
  it "goDownS on baseState with ANY node then come back up should be baseState" $ liftEffect porra

testGoUpS :: Spec Unit
testGoUpS = describe "goUpS tests" do
  it "goUpS on baseState should be baseState" do
    let newState = goUpS baseState
    newState `shouldEqual` baseState -- Passes

  it "goUpS after goDownS with 'mind' should be baseState" do
    let newState = goDownS "mind" baseState
        backState = goUpS newState
    backState `shouldEqual` baseState -- Passes

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "purescript-spec" do
    testGoUpS
    testGoDownS


-- testClick :: TestSuite
-- testClick = suite "Click tests" do
--   test "Clicking on 'mind' in baseState should update the state" do
--     -- Here you would simulate a click event on the "mind" node and then
--     -- check that the state updates as expected.
--     -- Depending on your actual codebase and how you've integrated with Halogen, 
--     -- the implementation of this test might look different.

--   test "Clicking on 'applied-ct' after 'mind' should update the state" do
--     -- Simulate a click event on the "applied-ct" node after "mind", and then
--     -- check that the state updates as expected.

--   test "Clicking on 'spivak' after 'mind' -> 'applied-ct' should update the state" do
--     -- Simulate a click event on the "spivak" node after "mind" -> "applied-ct", and then
--     -- check that the state updates as expected.

--   test "Clicking on 'applied-ct' after 'mind' -> 'applied-ct' -> 'spivak' should update the state and not render 'spivak'" do
--     -- Simulate a click event on the "applied-ct" node after "mind" -> "applied-ct" -> "spivak", and then
--     -- check that the state updates as expected and
