module Validator.IntDeclarationSpec where

import AST
import Control.Monad.Trans.State
import qualified Data.Set as Set
import Test.Hspec
import TestHelper (errorRegion, located, locatedE)
import Validator

spec :: Spec
spec = do
  describe "integer declaration validation" $ do
    it "should succeed if variable was not declared" $ do
      let statement = located (IntVar (located "foo") (located (Int 5)))
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult =
            ([], (snd initialState) {_variableSet = Set.singleton "foo"})
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    it "should fail if variable already exists" $ do
      let statement = located (IntVar (locatedE "foo") (located (Int 5)))
      let initialState =
            ( []
            , (emptyTable MainMethodScope) {_variableSet = Set.singleton "foo"})
      let expectedResult =
            ( [createErrorAt errorRegion VarAlreadyDeclaredError]
            , snd initialState)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
