module LexerSpec where

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
  describe "expression parser" $ do
    it "should parse all binary operations" $ do
      let parseInputs =
            map
              (\opStr ->
                 parseMaybe
                   expressionParser
                   ("HERE IS MY INVITATION 4 " ++ opStr ++ " b"))
              opStrings
            where
              opStrings = map fst ops
      let astResults =
            map (\astOp -> Just (BinaryOp astOp (IntConst 4) (Var "b"))) astOps
            where
              astOps = map snd ops
      parseInputs `shouldBe` astResults
    it "should parse chained binary operations" $
      -- (((4 + b) > 3) && 1) || 0
     do
      parseMaybe
        expressionParser
        "HERE IS MY INVITATION 4 \
        \GET UP b \
        \LET OFF SOME STEAM BENNET 3 \
        \KNOCK KNOCK @NO PROBLEMO \
        \CONSIDER THAT A DIVORCE @I LIED" `shouldBe`
        Just
          (BinaryOp
             Or
             (BinaryOp
                And
                (BinaryOp
                   GreaterThan
                   (BinaryOp Add (IntConst 4) (Var "b"))
                   (IntConst 3))
                (IntConst 1))
             (IntConst 0))
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
             (BinaryOp
                Divide
                (BinaryOp
                   Minus
                   (BinaryOp
                      Mult
                      (BinaryOp Add (IntConst 4) (Var "b"))
                      (IntConst 5))
                   (IntConst 1))
                (Var "send")))
