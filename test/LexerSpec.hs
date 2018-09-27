module LexerSpec (spec) where

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
            parseMaybe endParser "YOU HAVE BEEN TERMINATED" `shouldBe` Just "YOU HAVE BEEN TERMINATED"
        it "should fail to parse anything else" $ do
            parseMaybe endParser "SOME OTHER COMMAND" `shouldBe` Nothing        