module Validator.MethodSpec where

import AST
import qualified Data.Map as Map
import Test.Hspec
import TestHelper (errorRegion, located, locatedE)
import Validator

spec :: Spec
spec = do
  describe "nested scope validation" $ do
    it "should be possible to declare variables within blocks" $ do
      let method =
            Main
              [ (located $ IntVar (located "a") (located $ Int 0))
              , located $
                While
                  (located (Int 1))
                  [located $ IntVar (located "a") (located $ Int 0)]
              ]
      let symbolTable = emptyTable MainMethodScope
      validateMethod method symbolTable `shouldBe` []
  describe "non-void method return validation" $ do
    it "should succeed if last statement is a valid return statement" $ do
      let method =
            Method
              (located "foo")
              TInt
              []
              [ (located $ IntVar (located "a") (located $ Int 0))
              , located $ Return $ Just $ located $ Int 5
              ]
      let symbolTable = emptyTable GlobalScope
      validateMethod method symbolTable `shouldBe` []
    it "should fail if last statement is not a valid return statement" $ do
      let method =
            Method
              (located "foo")
              TInt
              []
              [(locatedE $ IntVar (located "a") (located $ Int 0))]
      let symbolTable = emptyTable GlobalScope
      validateMethod method symbolTable `shouldBe`
        [createErrorAt errorRegion ExpectingReturnStatementError]
  describe "method name validation" $ do
    it "should fail if a method is redeclared" $ do
      let program =
            [ Method
                (located "foo")
                TInt
                []
                [ (located $ IntVar (located "a") (located $ Int 0))
                , located $ Return $ Just $ located $ Int 5
                ]
            , Method
                (locatedE "foo")
                TVoid
                []
                [ (located $ IntVar (located "a") (located $ Int 0))
                , located $ Return $ Just $ located $ Int 5
                ]
            ]
      let symbolTable =
            (emptyTable GlobalScope) {_methodMap = Map.singleton "foo" TInt}
      createGlobalSymbolTable program `shouldBe`
        ([createErrorAt errorRegion MethodAlreadyDeclaredError], symbolTable)
  describe "arguments validation" $ do
    it "should succeed if all arguments are unique" $ do
      let method =
            Method
              (located "foo")
              TInt
              [located $ MethodArg "arg1", located $ MethodArg "arg2"]
              [located $ Return $ Just $ located $ Int 5]
      let symbolTable = emptyTable GlobalScope
      validateMethod method symbolTable `shouldBe` []
    it "should fail if there's a repeated argument" $ do
      let method =
            Method
              (located "foo")
              TInt
              [located $ MethodArg "arg1", locatedE $ MethodArg "arg1"]
              [located $ Return $ Just $ located $ Int 5]
      let symbolTable = emptyTable GlobalScope
      validateMethod method symbolTable `shouldBe`
        [createErrorAt errorRegion DuplicateArgumentError]
