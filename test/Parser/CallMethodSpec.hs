module Parser.CallMethodSpec where

import AST
import Parser
import qualified Region as R
import Test.Hspec
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
  describe "call method parser" $ do
    it "should parse a method without var assignment and no arguments" $ do
      parseMaybe callMethodStatementParser "DO IT NOW aMethod\n" `shouldBe`
        Just
          (CallMethod
             Nothing
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 1, R._column = 11}
                   , R._end = R.Position {R._line = 1, R._column = 18}
                   })
                "aMethod")
             [])
    it "should parse a method without var assignment and 2 arguments" $ do
      parseMaybe callMethodStatementParser "DO IT NOW aMethod 3 4\n" `shouldBe`
        Just
          (CallMethod
             Nothing
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 1, R._column = 11}
                   , R._end = R.Position {R._line = 1, R._column = 19}
                   })
                "aMethod")
             [ R.At
                 (R.Region
                    { R._start = R.Position {R._line = 1, R._column = 19}
                    , R._end = R.Position {R._line = 1, R._column = 21}
                    })
                 (Int 3)
             , R.At
                 (R.Region
                    { R._start = R.Position {R._line = 1, R._column = 21}
                    , R._end = R.Position {R._line = 1, R._column = 22}
                    })
                 (Int 4)
             ])
    it "should parse a method with var assignment and no arguments" $ do
      parseMaybe
        callMethodStatementParser
        "GET YOUR ASS TO MARS someVar\n\
        \DO IT NOW aMethod\n" `shouldBe`
        Just
          (CallMethod
             (Just
                (R.At
                   (R.Region
                      { R._start = R.Position {R._line = 1, R._column = 22}
                      , R._end = R.Position {R._line = 1, R._column = 29}
                      })
                   "someVar"))
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 2, R._column = 11}
                   , R._end = R.Position {R._line = 2, R._column = 18}
                   })
                "aMethod")
             [])
    it "should parse a method with var assignment and 3 arguments" $ do
      parseMaybe
        callMethodStatementParser
        "GET YOUR ASS TO MARS someVar\n\
        \DO IT NOW aMethod 10 @I LIED b \n" `shouldBe`
        Just
          (CallMethod
             (Just
                (R.At
                   (R.Region
                      { R._start = R.Position {R._line = 1, R._column = 22}
                      , R._end = R.Position {R._line = 1, R._column = 29}
                      })
                   "someVar"))
             (R.At
                (R.Region
                   { R._start = R.Position {R._line = 2, R._column = 11}
                   , R._end = R.Position {R._line = 2, R._column = 19}
                   })
                "aMethod")
             [ R.At
                 (R.Region
                    { R._start = R.Position {R._line = 2, R._column = 19}
                    , R._end = R.Position {R._line = 2, R._column = 22}
                    })
                 (Int 10)
             , R.At
                 (R.Region
                    { R._start = R.Position {R._line = 2, R._column = 22}
                    , R._end = R.Position {R._line = 2, R._column = 30}
                    })
                 (Int 0)
             , R.At
                 (R.Region
                    { R._start = R.Position {R._line = 2, R._column = 30}
                    , R._end = R.Position {R._line = 2, R._column = 32}
                    })
                 (Var "b")
             ])
