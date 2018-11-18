module Validator.ConditionSpec where

import AST
import Control.Monad.Trans.State
import Test.Hspec
import TestHelper (errorRegion, located, locatedE)
import Validator

spec :: Spec
spec = do
  describe "if-else condition validation" $ do
    it "should succeed if condition expression is valid" $ do
      let statement = located $ If (located (Int 1)) [] []
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult = ([], emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
    it "should fail if condition expression is invalid" $ do
      let statement = located $ If (locatedE (Var "foo")) [] []
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult =
            ( [createErrorAt errorRegion VarNotDeclaredError]
            , emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
  describe "if statements validation" $ do
    it "should succeed if statements are valid" $ do
      let statement =
            located
              (If (located (Int 1)) [(located (PrintStr "hello world"))] [])
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult = ([], emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    it "should fail if statements are invalid" $ do
      let statement =
            located
              (If
                 (located (Int 1))
                 [(locatedE (PrintExpr $ locatedE (Var "foo")))]
                 [])
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult =
            ( [createErrorAt errorRegion VarNotDeclaredError]
            , emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
  describe "else statements validation" $ do
    it "should succeed if statements are valid" $ do
      let statement =
            located
              (If (located (Int 1)) [] [(located (PrintStr "hello world"))])
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult = ([], emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    it "should fail if statements are invalid" $ do
      let statement =
            located
              (If
                 (located (Int 1))
                 []
                 [(locatedE (PrintExpr $ locatedE (Var "foo")))])
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult =
            ( [createErrorAt errorRegion VarNotDeclaredError]
            , emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
  describe "while condition validation" $ do
    it "should succeed if condition expression is valid" $ do
      let statement = located $ While (located (Int 1)) []
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult = ([], emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
    it "should fail if condition expression is invalid" $ do
      let statement = located $ While (locatedE (Var "foo")) []
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult =
            ( [createErrorAt errorRegion VarNotDeclaredError]
            , emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
  describe "while statements validation" $ do
    it "should succeed if statements are valid" $ do
      let statement =
            located
              (While (located (Int 1)) [(located (PrintStr "hello world"))])
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult = ([], emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    it "should fail if statements are invalid" $ do
      let statement =
            located
              (While
                 (located (Int 1))
                 [(locatedE (PrintExpr $ locatedE (Var "foo")))])
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult =
            ( [createErrorAt errorRegion VarNotDeclaredError]
            , emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
