module Validator.AssignmentSpec where

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
  describe "assignment declaration validation" $ do
    it "should succeed if target variable was declared" $ do
      let statement =
            (R.At
               dummyRegion
               (Assignment (R.At dummyRegion "foo") (R.At dummyRegion (Int 4))))
      let initialState =
            ([], (emptyTable "") {_variableSet = Set.singleton "foo"})
      let expectedResult =
            ( []
            , SymbolTable
                { _parentTable = Nothing
                , _variableSet = Set.singleton "foo"
                , _methodSet = Set.empty
                , _currentMethod = ""
                })
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    --  
    it "should fail if target variable was not declared" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 29}
              , R._end = R.Position {R._line = 5, R._column = 37}
              }
      let statement =
            (R.At
               dummyRegion
               (Assignment (R.At errorRegion "foo") (R.At dummyRegion (Int 4))))
      let initialState = ([], emptyTable "")
      let expectedResult =
            ( [Error {_location = errorRegion, _errorMsg = VarNotDeclaredError}]
            , emptyTable "")
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
    --
    it "should fail if expression validation fails" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 36}
              , R._end = R.Position {R._line = 5, R._column = 37}
              }
      let statement =
            (R.At
               dummyRegion
               (Assignment (R.At dummyRegion "foo") (R.At errorRegion (Var "a"))))
      let symbolTable = (emptyTable "") {_variableSet = Set.singleton "foo"}
      let initialState = ([], symbolTable)
      let expectedResult =
            ( [Error {_location = errorRegion, _errorMsg = VarNotDeclaredError}]
            , symbolTable)
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
            (R.At
               dummyRegion
               (Assignment
                  (R.At errorRegion1 "foo")
                  (R.At errorRegion2 (Var "a"))))
      let symbolTable = emptyTable ""
      let initialState = ([], symbolTable)
      let expectedResult =
            ( [ Error
                  {_location = errorRegion1, _errorMsg = VarNotDeclaredError}
              , Error
                  {_location = errorRegion2, _errorMsg = VarNotDeclaredError}
              ]
            , symbolTable)
      runState (validateStatement statement) initialState `shouldBe`
        ((), expectedResult)
