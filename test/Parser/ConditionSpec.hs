module Parser.ConditionSpec where

import AST
import Parser
import qualified Region as R
import Test.Hspec
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
  describe "if-else parser" $ do
    it "should parse if statements without else" $ do
      parseMaybe
        ifStatementParser
        "BECAUSE I'M GOING TO SAY PLEASE a\n\
        \TALK TO THE HAND \"a is true\"\n\
        \TALK TO THE HAND \"a' is unknown\"\n\
        \YOU HAVE NO RESPECT FOR LOGIC\n" `shouldBe`
        Just
          (If
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 1, R._column = 33}
                   , R._end = R.Position {R._line = 1, R._column = 34}
                   })
                (Var "a"))
             [ R.At
                 (R.Region
                    { R._start = R.Position {R._line = 2, R._column = 1}
                    , R._end = R.Position {R._line = 3, R._column = 1}
                    })
                 (PrintStr "a is true")
             , R.At
                 (R.Region
                    { R._start = R.Position {R._line = 3, R._column = 1}
                    , R._end = R.Position {R._line = 4, R._column = 1}
                    })
                 (PrintStr "a' is unknown")
             ]
             [])
    it "should parse if statements with else" $ do
      parseMaybe
        ifStatementParser
        "BECAUSE I'M GOING TO SAY PLEASE a\n\
        \TALK TO THE HAND \"a is true\"\n\
        \BULLSHIT\n\
        \TALK TO THE HAND \"a is not true\"\n\
        \TALK TO THE HAND \"a' might be\"\n\
        \YOU HAVE NO RESPECT FOR LOGIC\n" `shouldBe`
        Just
          (If
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 1, R._column = 33}
                   , R._end = R.Position {R._line = 1, R._column = 34}
                   })
                (Var "a"))
             [ R.At
                 (R.Region
                    { R._start = R.Position {R._line = 2, R._column = 1}
                    , R._end = R.Position {R._line = 3, R._column = 1}
                    })
                 (PrintStr "a is true")
             ]
             [ R.At
                 (R.Region
                    { R._start = R.Position {R._line = 4, R._column = 1}
                    , R._end = R.Position {R._line = 5, R._column = 1}
                    })
                 (PrintStr "a is not true")
             , R.At
                 (R.Region
                    { R._start = R.Position {R._line = 5, R._column = 1}
                    , R._end = R.Position {R._line = 6, R._column = 1}
                    })
                 (PrintStr "a' might be")
             ])
    it "should parse empty if and else statements" $ do
      parseMaybe
        ifStatementParser
        "BECAUSE I'M GOING TO SAY PLEASE a\n\
        \BULLSHIT\n\
        \YOU HAVE NO RESPECT FOR LOGIC\n" `shouldBe`
        Just
          (If
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 1, R._column = 33}
                   , R._end = R.Position {R._line = 1, R._column = 34}
                   })
                (Var "a"))
             []
             [])
  describe "while parser" $ do
    it "should parse while statements" $ do
      parseMaybe
        whileStatementParser
        "STICK AROUND a\n\
        \TALK TO THE HAND \"a is true\"\n\
        \CHILL" `shouldBe`
        Just
          (While
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 1, R._column = 14}
                   , R._end = R.Position {R._line = 1, R._column = 15}
                   })
                (Var "a"))
             [ R.At
                 (R.Region
                    { R._start = R.Position {R._line = 2, R._column = 1}
                    , R._end = R.Position {R._line = 3, R._column = 1}
                    })
                 (PrintStr "a is true")
             ])
