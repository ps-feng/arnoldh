module Validator.ReturnSpec where

import AST
import Control.Monad.Trans.State
import qualified Data.Set as Set
import Test.Hspec
import TestHelper (errorRegion, located, locatedE)
import Validator

spec :: Spec
spec = do
  describe "return validation" $ do
    it "should succeed if returns an existing variable" $ do
      let statement = located (Return $ Just $ located $ Var "a")
      let initialState =
            ( []
            , (emptyTable $ MethodScope "foo" TInt)
                {_variableSet = Set.singleton "a"})
      let expectedResult = initialState
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    --
    it "should fail if returns an inexistent variable" $ do
      let statement = located (Return $ Just $ locatedE $ Var "a")
      let parentTable = emptyTable MainMethodScope
      let initialState =
            ( []
            , (emptyTable $ MethodScope "foo" TInt)
                {_parentTable = Just parentTable})
      let expectedResult =
            ([createErrorAt errorRegion VarNotDeclaredError], snd initialState)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    --
    it "should fail if returns a value in a void function" $ do
      let statement = located (Return $ Just $ locatedE $ Int 5)
      let parentTable = emptyTable MainMethodScope
      let initialState =
            ( []
            , (emptyTable $ MethodScope "foo" TVoid)
                {_parentTable = Just parentTable})
      let expectedResult =
            ( [createErrorAt errorRegion ReturnsValueInVoidMethodError]
            , snd initialState)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    --
    it "should fail if doesn't return a value in a non-void function" $ do
      let statement = locatedE (Return Nothing)
      let initialState = ([], (emptyTable $ MethodScope "foo" TInt))
      let expectedResult =
            ( [createErrorAt errorRegion MissingReturnValueInNonVoidMethodError]
            , snd initialState)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    --
    it "should fail if returning within main method" $ do
      let statement = locatedE (Return Nothing)
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult =
            ( [createErrorAt errorRegion IllegalReturnMethodError]
            , snd initialState)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
