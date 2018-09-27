module LexerSpec
  ( spec
  ) where

import AST
import Lexer
import Test.Hspec
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
  describe "beginParser" $ do
    it "should parse the begin command" $ do
      parseMaybe beginParser "IT'S SHOWTIME" `shouldBe` Just "IT'S SHOWTIME"
    it "should fail to parse anything else" $ do
      parseMaybe beginParser "SOME OTHER COMMAND" `shouldBe` Nothing
  describe "endParser" $ do
    it "should parse the end command" $ do
      parseMaybe endParser "YOU HAVE BEEN TERMINATED" `shouldBe`
        Just "YOU HAVE BEEN TERMINATED"
    it "should fail to parse anything else" $ do
      parseMaybe endParser "SOME OTHER COMMAND" `shouldBe` Nothing
  describe "assignment parser" $ do
    it "should fail if assignment statements are not provided" $ do
      parseMaybe
        assignmentParser
        "GET TO THE CHOPPER myvar \
          \ENOUGH TALK" `shouldBe`
        Nothing
    it "should parse a single integer assignment" $ do
      parseMaybe
        assignmentParser
        "GET TO THE CHOPPER myvar \
        \HERE IS MY INVITATION 4 \
        \ENOUGH TALK" `shouldBe`
        Just (Assignment "myvar" (IntConst 4))
    it "should parse a variable assignment" $ do
      parseMaybe
        assignmentParser
        "GET TO THE CHOPPER myvar \
        \HERE IS MY INVITATION a \
        \ENOUGH TALK" `shouldBe`
        Just (Assignment "myvar" (Var "a"))
    it "should parse a list of operations" $ do
      parseMaybe
        assignmentParser
        "GET TO THE CHOPPER myvar \
        \HERE IS MY INVITATION 4 \
        \GET UP b \
        \YOU'RE FIRED 5 \
        \GET DOWN 1 \
        \HE HAD TO SPLIT send \
        \ENOUGH TALK" `shouldBe`
        Just
          (Assignment
             "myvar"
             (ArithBinary
                Divide
                (ArithBinary
                   Minus
                   (ArithBinary
                      Mult
                      (ArithBinary Add (IntConst 4) (Var "b"))
                      (IntConst 5))
                   (IntConst 1))
                (Var "send")))
