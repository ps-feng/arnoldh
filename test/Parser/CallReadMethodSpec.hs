module Parser.CallReadMethodSpec where

import AST
import Parser
import qualified Region as R
import Test.Hspec
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
  describe "call read method parser" $ do
    it "should parse it correctly" $ do
      parseMaybe
        callReadMethodStatementParser
        "GET YOUR ASS TO MARS var\n\
        \DO IT NOW\n\
        \I WANT TO ASK YOU A BUNCH OF QUESTIONS AND I WANT TO HAVE THEM ANSWERED IMMEDIATELY\n" `shouldBe`
        Just
          (CallRead
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 1, R._column = 22}
                   , R.end = R.Position {R._line = 1, R._column = 25}
                   })
                "var"))
