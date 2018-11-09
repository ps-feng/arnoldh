module Validator.ConditionSpec where

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
  describe "if-else condition validation" $ do
    it "should succeed if condition expression is valid" $ do
      let statement = R.At dummyRegion $ If (R.At dummyRegion (Int 1)) [] []
      let initialState = ([], emptyTable "")
      let expectedResult = ([], emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
    it "should fail if condition expression is invalid" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 29}
              , R._end = R.Position {R._line = 5, R._column = 37}
              }
      let statement = R.At dummyRegion $ If (R.At errorRegion (Var "foo")) [] []
      let initialState = ([], emptyTable "")
      let expectedResult =
            ( [Error {_location = errorRegion, _errorMsg = VarNotDeclaredError}]
            , emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
  describe "if statements validation" $ do
    it "should succeed if statements are valid" $ do
      let statement =
            R.At
              dummyRegion
              (If
                 (R.At dummyRegion (Int 1))
                 [(R.At dummyRegion (PrintStr "hello world"))]
                 [])
      let initialState = ([], emptyTable "")
      let expectedResult = ([], emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    it "should fail if statements are invalid" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 29}
              , R._end = R.Position {R._line = 5, R._column = 37}
              }
      let statement =
            R.At
              dummyRegion
              (If
                 (R.At dummyRegion (Int 1))
                 [(R.At errorRegion (PrintExpr $ R.At errorRegion (Var "foo")))]
                 [])
      let initialState = ([], emptyTable "")
      let expectedResult =
            ( [Error {_location = errorRegion, _errorMsg = VarNotDeclaredError}]
            , emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
  describe "else statements validation" $ do
    it "should succeed if statements are valid" $ do
      let statement =
            R.At
              dummyRegion
              (If
                 (R.At dummyRegion (Int 1))
                 []
                 [(R.At dummyRegion (PrintStr "hello world"))])
      let initialState = ([], emptyTable "")
      let expectedResult = ([], emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    it "should fail if statements are invalid" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 29}
              , R._end = R.Position {R._line = 5, R._column = 37}
              }
      let statement =
            R.At
              dummyRegion
              (If
                 (R.At dummyRegion (Int 1))
                 []
                 [(R.At errorRegion (PrintExpr $ R.At errorRegion (Var "foo")))])
      let initialState = ([], emptyTable "")
      let expectedResult =
            ( [Error {_location = errorRegion, _errorMsg = VarNotDeclaredError}]
            , emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
  describe "while condition validation" $ do
    it "should succeed if condition expression is valid" $ do
      let statement = R.At dummyRegion $ While (R.At dummyRegion (Int 1)) []
      let initialState = ([], emptyTable "")
      let expectedResult = ([], emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
    it "should fail if condition expression is invalid" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 29}
              , R._end = R.Position {R._line = 5, R._column = 37}
              }
      let statement = R.At dummyRegion $ While (R.At errorRegion (Var "foo")) []
      let initialState = ([], emptyTable "")
      let expectedResult =
            ( [Error {_location = errorRegion, _errorMsg = VarNotDeclaredError}]
            , emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
  describe "while statements validation" $ do
    it "should succeed if statements are valid" $ do
      let statement =
            R.At
              dummyRegion
              (While
                 (R.At dummyRegion (Int 1))
                 [(R.At dummyRegion (PrintStr "hello world"))])
      let initialState = ([], emptyTable "")
      let expectedResult = ([], emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    it "should fail if statements are invalid" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 29}
              , R._end = R.Position {R._line = 5, R._column = 37}
              }
      let statement =
            R.At
              dummyRegion
              (While
                 (R.At dummyRegion (Int 1))
                 [(R.At errorRegion (PrintExpr $ R.At errorRegion (Var "foo")))])
      let initialState = ([], emptyTable "")
      let expectedResult =
            ( [Error {_location = errorRegion, _errorMsg = VarNotDeclaredError}]
            , emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
