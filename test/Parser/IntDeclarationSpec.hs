module Parser.IntDeclarationSpec where

import AST
import Parser
import qualified Region as R
import Test.Hspec
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
  describe "int declaration parser" $ do
    it "should parse signed integer" $ do
      parseMaybe
        intVarDeclarationStatementParser
        "HEY CHRISTMAS TREE someVar\n\
        \YOU SET US UP -5\n" `shouldBe`
        Just
          (IntVar
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 1, R._column = 20}
                   , R._end = R.Position {R._line = 1, R._column = 27}
                   })
                "someVar")
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 2, R._column = 15}
                   , R._end = R.Position {R._line = 2, R._column = 17}
                   })
                (Int (-5))))
    it "should not parse a string" $ do
      parseMaybe
        intVarDeclarationStatementParser
        "HEY CHRISTMAS TREE someVar\n\
        \YOU SET US UP \"hola\"\n" `shouldBe`
        Nothing
    it "should parse a variable" $ do
      parseMaybe
        intVarDeclarationStatementParser
        "HEY CHRISTMAS TREE someVar\n\
        \YOU SET US UP a\n" `shouldBe`
        Just
          (IntVar
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 1, R._column = 20}
                   , R._end = R.Position {R._line = 1, R._column = 27}
                   })
                "someVar")
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 2, R._column = 15}
                   , R._end = R.Position {R._line = 2, R._column = 16}
                   })
                (Var "a")))
    it "should parse boolean" $ do
      parseMaybe
        intVarDeclarationStatementParser
        "HEY CHRISTMAS TREE someVar\n\
        \YOU SET US UP @I LIED\n" `shouldBe`
        Just
          (IntVar
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 1, R._column = 20}
                   , R._end = R.Position {R._line = 1, R._column = 27}
                   })
                "someVar")
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 2, R._column = 15}
                   , R._end = R.Position {R._line = 2, R._column = 22}
                   })
                (Int 0)))
    it "should not parse a number as a variable" $ do
      parseMaybe
        intVarDeclarationStatementParser
        "HEY CHRISTMAS TREE 4\n\
        \YOU SET US UP\n" `shouldBe`
        Nothing
