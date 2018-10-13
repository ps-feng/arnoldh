module Parser.PrintSpec where

import AST
import Parser
import qualified Region as R
import Test.Hspec
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
  describe "print statement parser" $ do
    it "should parse double-quoted strings" $ do
      parseMaybe printStatementParser "TALK TO THE HAND \"Hello\"\n" `shouldBe`
        Just (PrintStr "Hello")
    it "should not parse single-quoted strings" $ do
      parseMaybe printStatementParser "TALK TO THE HAND \'Hello\'\n" `shouldBe`
        Nothing
    it "should parse printing of an integer" $ do
      parseMaybe printStatementParser "TALK TO THE HAND 4\n" `shouldBe`
        Just
          (PrintExpr
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 1, R._column = 18}
                   , R.end = R.Position {R._line = 1, R._column = 19}
                   })
                (Int 4)))
    it "should parse a variable reference" $ do
      parseMaybe printStatementParser "TALK TO THE HAND a\n" `shouldBe`
        Just
          (PrintExpr
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 1, R._column = 18}
                   , R.end = R.Position {R._line = 1, R._column = 19}
                   })
                (Var "a")))
