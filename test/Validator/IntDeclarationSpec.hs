module Validator.IntDeclarationSpec where

import AST
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
  describe "integer declaration validation" $ do
    it "should succeed if variable is not used" $ do
      let method =
            Main
              [ (R.At
                   dummyRegion
                   (IntVar (R.At dummyRegion "foo") (R.At dummyRegion (Int 5))))
              ]
      let expectedResult =
            Right
              (SymbolTable
                 { _parentTable = Nothing
                 , _variableSet = Set.singleton "foo"
                 , _methodSet = Set.empty
                 , _currentMethod = "main"
                 })
      validateMethod method (emptyTable "main") `shouldBe` expectedResult
    it "should fail if variable already exists" $ do
      let errorRegion =
            R.Region
              { R._start = R.Position {R._line = 5, R._column = 38}
              , R._end = R.Position {R._line = 5, R._column = 40}
              }
      let method =
            Main
              [ (R.At
                   dummyRegion
                   (IntVar (R.At errorRegion "foo") (R.At dummyRegion (Int 5))))
              ]
      let symbolTable = (emptyTable "main") {_variableSet = Set.singleton "foo"}
      let expectedResult =
            Left
              [ Error
                  {_location = errorRegion, _errorMsg = VarAlreadyDeclaredError}
              ]
      validateMethod method symbolTable `shouldBe` expectedResult
