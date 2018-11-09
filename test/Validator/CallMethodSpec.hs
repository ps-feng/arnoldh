module Validator.CallMethodSpec where

import AST
import Control.Monad.Trans.State
import qualified Data.Set as Set
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
  describe "called method name validation" $ do
    it "should succeed if method exists" $ do
      let statement =
            R.At dummyRegion $ CallMethod Nothing (R.At dummyRegion "foo") []
      let initialState =
            ([], (emptyTable "") {_methodSet = Set.singleton "foo"})
      let expectedResult = initialState
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
    it "should fail if method does not exist" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 29}
              , R._end = R.Position {R._line = 5, R._column = 32}
              }
      let statement =
            R.At dummyRegion $ CallMethod Nothing (R.At errorRegion "foo") []
      let initialState = ([], emptyTable "")
      let expectedResult =
            ( [Error {_location = errorRegion, _errorMsg = MethodNotDeclaredError}]
            , emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
  describe "arguments validation" $ do
    it "should succeed if arguments are valid" $ do
      let statement =
            R.At dummyRegion $
            CallMethod
              Nothing
              (R.At dummyRegion "foo")
              [(R.At dummyRegion $ Int 4), (R.At dummyRegion $ Var "a")]
      let initialState =
            ( []
            , (emptyTable "")
                { _methodSet = Set.singleton "foo"
                , _variableSet = Set.singleton "a"
                })
      let expectedResult = initialState
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    it "should fail if arguments are invalid" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 29}
              , R._end = R.Position {R._line = 5, R._column = 32}
              }
      let statement =
            R.At dummyRegion $
            CallMethod
              Nothing
              (R.At dummyRegion "foo")
              [(R.At dummyRegion $ Int 4), (R.At errorRegion $ Var "a")]
      let initialState =
            ([], (emptyTable "") {_methodSet = Set.singleton "foo"})
      let expectedResult =
            ( [Error {_location = errorRegion, _errorMsg = VarNotDeclaredError}]
            , (emptyTable "") {_methodSet = Set.singleton "foo"})
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
  describe "storage variable validation" $ do
    it "should succeed if variable exists" $ do
      let statement =
            R.At dummyRegion $
            CallMethod (Just $ R.At dummyRegion "a") (R.At dummyRegion "foo") []
      let initialState =
            ( []
            , (emptyTable "")
                { _methodSet = Set.singleton "foo"
                , _variableSet = Set.singleton "a"
                })
      let expectedResult = initialState
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
    it "should fail if variable does not exist" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 29}
              , R._end = R.Position {R._line = 5, R._column = 32}
              }
      let statement =
            R.At dummyRegion $
            CallMethod (Just $ R.At errorRegion "a") (R.At dummyRegion "foo") []
      let initialState =
            ([], (emptyTable "") {_methodSet = Set.singleton "foo"})
      let expectedResult =
            ( [Error {_location = errorRegion, _errorMsg = VarNotDeclaredError}]
            , (emptyTable "") {_methodSet = Set.singleton "foo"})
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
