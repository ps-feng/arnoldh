module Validator.AssignmentSpec where

import AST
import Control.Monad.Trans.State
import qualified Data.Set as Set
import qualified Region as R
import Test.Hspec
import TestHelper (errorRegion, located, locatedE)
import Validator

spec :: Spec
spec = do
  describe "assignment declaration validation" $ do
    it "should succeed if target variable was declared" $ do
      let statement = (located (Assignment (located "foo") (located (Int 4))))
      let symbolTable =
            (emptyTable MainMethodScope) {_variableSet = Set.singleton "foo"}
      let initialState = ([], symbolTable)
      runState (validateStatement statement) initialState `shouldBe`
        ((), initialState)
    --  
    it "should fail if target variable was not declared" $ do
      let statement = (located (Assignment (locatedE "foo") (located (Int 4))))
      let symbolTable = emptyTable MainMethodScope
      let initialState = ([], symbolTable)
      let expectedResult =
            ([createErrorAt errorRegion VarNotDeclaredError], symbolTable)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    --
    it "should fail if expression validation fails" $ do
      let statement =
            (located (Assignment (located "foo") (locatedE (Var "a"))))
      let symbolTable =
            (emptyTable MainMethodScope) {_variableSet = Set.singleton "foo"}
      let initialState = ([], symbolTable)
      let expectedResult =
            ([createErrorAt errorRegion VarNotDeclaredError], symbolTable)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    --
    it "should accumulate validation failures" $ do
      let errorRegion1 =
            R.Region
              { R._start = R.Position {R._line = 4, R._column = 25}
              , R._end = R.Position {R._line = 4, R._column = 28}
              }
      let errorRegion2 =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 36}
              , R._end = R.Position {R._line = 5, R._column = 37}
              }
      let statement =
            (located
               (Assignment
                  (R.At errorRegion1 "foo")
                  (R.At errorRegion2 (Var "a"))))
      let symbolTable = emptyTable MainMethodScope
      let initialState = ([], symbolTable)
      let expectedResult =
            ( [ createErrorAt errorRegion1 VarNotDeclaredError
              , createErrorAt errorRegion2 VarNotDeclaredError
              ]
            , symbolTable)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
