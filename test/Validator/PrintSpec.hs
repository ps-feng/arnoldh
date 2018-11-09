module Validator.PrintSpec where

import AST
import Control.Monad.Trans.State
import qualified Region as R
import Test.Hspec
import Validator

dummyRegion :: R.Region
dummyRegion =
  R.Region
    { R._start = R.Position {R._line = 1, R._column = 1}
    , R._end = R.Position {R._line = 1, R._column = 2}
    }

spec :: Spec
spec = do
  describe "print expression validation" $ do
    it "should succeed if expression is valid" $ do
      let statement = (R.At dummyRegion (PrintExpr (R.At dummyRegion (Int 4))))
      let initialState = ([], emptyTable "")
      let expectedResult = ([], emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    --
    it "should fail if expression is not valid" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 29}
              , R._end = R.Position {R._line = 5, R._column = 37}
              }
      let statement =
            (R.At dummyRegion (PrintExpr (R.At errorRegion (Var "a"))))
      let initialState = ([], emptyTable "")
      let expectedResult =
            ( [Error {_location = errorRegion, _errorMsg = VarNotDeclaredError}]
            , emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
  --
  describe "print string validation" $ do
    it "should succeed without errors" $ do
      let statement = (R.At dummyRegion (PrintStr "hello world"))
      let initialState = ([], emptyTable "")
      let expectedResult = ([], emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
