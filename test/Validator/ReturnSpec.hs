module Validator.ReturnSpec where

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
  describe "return validation" $ do
    it "should succeed if returned expression is valid" $ do
      let statement =
            R.At dummyRegion (Return $ Just $ R.At dummyRegion $ Var "a")
      let initialState =
            ([], (emptyTable "") {_variableSet = Set.singleton "a"})
      let expectedResult = initialState
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
      --
    it "should fail if returned expression is not valid" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 29}
              , R._end = R.Position {R._line = 5, R._column = 30}
              }
      let statement =
            R.At dummyRegion (Return $ Just $ R.At errorRegion $ Var "a")
      let initialState = ([], (emptyTable ""))
      let expectedResult =
            ( [Error {_location = errorRegion, _errorMsg = VarNotDeclaredError}]
            , snd initialState)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
