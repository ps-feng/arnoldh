module Validator.CallMethodSpec where

import AST
import Control.Monad.Trans.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec
import TestHelper (errorRegion, located, locatedE)
import Validator

spec :: Spec
spec = do
  describe "called method name validation" $ do
    it "should succeed if method exists" $ do
      let statement = located $ CallMethod Nothing (located "foo") []
      let parentTable =
            (emptyTable GlobalScope) {_methodMap = Map.singleton "foo" TVoid}
      let initialState =
            ([], (emptyTable MainMethodScope) {_parentTable = Just parentTable})
      let expectedResult = initialState
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
    it "should fail if method does not exist" $ do
      let statement = located $ CallMethod Nothing (locatedE "foo") []
      let initialState =
            ( []
            , (emptyTable MainMethodScope)
                {_parentTable = Just $ emptyTable GlobalScope})
      let expectedResult =
            ([createErrorAt errorRegion MethodNotDeclaredError], snd initialState)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
  describe "arguments validation" $ do
    it "should succeed if arguments are valid" $ do
      let statement =
            located $
            CallMethod
              Nothing
              (located "foo")
              [(located $ Int 4), (located $ Var "a")]
      let parentTable =
            (emptyTable GlobalScope) {_methodMap = Map.singleton "foo" TVoid}
      let initialState =
            ( []
            , (emptyTable MainMethodScope)
                { _parentTable = Just $ parentTable
                , _variableSet = Set.singleton "a"
                })
      let expectedResult = initialState
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    it "should fail if arguments are invalid" $ do
      let statement =
            located $
            CallMethod
              Nothing
              (located "foo")
              [(located $ Int 4), (locatedE $ Var "a")]
      let parentTable =
            (emptyTable GlobalScope) {_methodMap = Map.singleton "foo" TVoid}
      let initialState =
            ( []
            , (emptyTable MainMethodScope) {_parentTable = Just $ parentTable})
      let expectedResult =
            ([createErrorAt errorRegion VarNotDeclaredError], snd initialState)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
  describe "storage variable validation" $ do
    it "should succeed if variable exists" $ do
      let statement =
            located $ CallMethod (Just $ located "a") (located "foo") []
      let parentTable =
            (emptyTable GlobalScope) {_methodMap = Map.singleton "foo" TInt}
      let initialState =
            ( []
            , (emptyTable MainMethodScope)
                { _parentTable = Just parentTable
                , _variableSet = Set.singleton "a"
                })
      let expectedResult = initialState
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
    it "should fail if variable does not exist" $ do
      let statement =
            located $ CallMethod (Just $ locatedE "a") (located "foo") []
      let parentTable =
            (emptyTable GlobalScope) {_methodMap = Map.singleton "foo" TInt}
      let initialState =
            ( []
            , (emptyTable MainMethodScope) {_parentTable = Just $ parentTable})
      let expectedResult =
            ([createErrorAt errorRegion VarNotDeclaredError], snd initialState)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    it "should fail if trying to store a variable when the method is void" $ do
      let statement =
            located $ CallMethod (Just $ located "a") (locatedE "foo") []
      let parentTable =
            (emptyTable GlobalScope) {_methodMap = Map.singleton "foo" TVoid}
      let initialState =
            ( []
            , (emptyTable MainMethodScope)
                { _parentTable = Just parentTable
                , _variableSet = Set.singleton "a"
                })
      let expectedResult =
            ( [createErrorAt errorRegion ExpectingNonVoidMethodError]
            , snd initialState)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
