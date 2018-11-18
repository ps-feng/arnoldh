module Validator.PrintSpec where

import AST
import Control.Monad.Trans.State
import Test.Hspec
import TestHelper (errorRegion, located, locatedE)
import Validator

spec :: Spec
spec = do
  describe "print expression validation" $ do
    it "should succeed if expression is valid" $ do
      let statement = (located (PrintExpr (located (Int 4))))
      let initialState = ([], emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), initialState)
    --
    it "should fail if expression is not valid" $ do
      let statement = (located (PrintExpr (locatedE (Var "a"))))
      let initialState = ([], emptyTable MainMethodScope)
      let expectedResult =
            ( [createErrorAt errorRegion VarNotDeclaredError]
            , emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
  --
  describe "print string validation" $ do
    it "should succeed without errors" $ do
      let statement = (located (PrintStr "hello world"))
      let initialState = ([], emptyTable MainMethodScope)
      runState (validateStatement statement) initialState `shouldBe`
        ((), initialState)
