module Parser.ExpressionSpec where

import AST
import Parser
import qualified Region as R
import Test.Hspec
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
  describe "expression parser" $ do
    it "should parse add operation" $ do
      parseMaybe
        expressionParser
        "HERE IS MY INVITATION 4\n\
        \GET UP b\n" `shouldBe`
        Just
          (R.At
             (R.Region
                { R.start = R.Position {R._line = 2, R._column = 1}
                , R.end = R.Position {R._line = 2, R._column = 8}
                })
             (BinaryOp
                Add
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 1, R._column = 23}
                      , R.end = R.Position {R._line = 1, R._column = 24}
                      })
                   (Int 4))
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 2, R._column = 8}
                      , R.end = R.Position {R._line = 2, R._column = 9}
                      })
                   (Var "b"))))
    it "should parse minus operation" $ do
      parseMaybe
        expressionParser
        "HERE IS MY INVITATION 4\n\
        \GET DOWN b\n" `shouldBe`
        Just
          (R.At
             (R.Region
                { R.start = R.Position {R._line = 2, R._column = 1}
                , R.end = R.Position {R._line = 2, R._column = 10}
                })
             (BinaryOp
                Minus
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 1, R._column = 23}
                      , R.end = R.Position {R._line = 1, R._column = 24}
                      })
                   (Int 4))
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 2, R._column = 10}
                      , R.end = R.Position {R._line = 2, R._column = 11}
                      })
                   (Var "b"))))
    it "should parse divide operation" $ do
      parseMaybe
        expressionParser
        "HERE IS MY INVITATION 4\n\
        \HE HAD TO SPLIT b\n" `shouldBe`
        Just
          (R.At
             (R.Region
                { R.start = R.Position {R._line = 2, R._column = 1}
                , R.end = R.Position {R._line = 2, R._column = 17}
                })
             (BinaryOp
                Divide
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 1, R._column = 23}
                      , R.end = R.Position {R._line = 1, R._column = 24}
                      })
                   (Int 4))
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 2, R._column = 17}
                      , R.end = R.Position {R._line = 2, R._column = 18}
                      })
                   (Var "b"))))
    it "should parse multiplication operation" $ do
      parseMaybe
        expressionParser
        "HERE IS MY INVITATION 4\n\
        \YOU'RE FIRED b\n" `shouldBe`
        Just
          (R.At
             (R.Region
                { R.start = R.Position {R._line = 2, R._column = 1}
                , R.end = R.Position {R._line = 2, R._column = 14}
                })
             (BinaryOp
                Mult
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 1, R._column = 23}
                      , R.end = R.Position {R._line = 1, R._column = 24}
                      })
                   (Int 4))
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 2, R._column = 14}
                      , R.end = R.Position {R._line = 2, R._column = 15}
                      })
                   (Var "b"))))
    it "should parse modulo operation" $ do
      parseMaybe
        expressionParser
        "HERE IS MY INVITATION 4\n\
        \I LET HIM GO b\n" `shouldBe`
        Just
          (R.At
             (R.Region
                { R.start = R.Position {R._line = 2, R._column = 1}
                , R.end = R.Position {R._line = 2, R._column = 14}
                })
             (BinaryOp
                Modulo
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 1, R._column = 23}
                      , R.end = R.Position {R._line = 1, R._column = 24}
                      })
                   (Int 4))
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 2, R._column = 14}
                      , R.end = R.Position {R._line = 2, R._column = 15}
                      })
                   (Var "b"))))
    it "should parse 'or' operation" $ do
      parseMaybe
        expressionParser
        "HERE IS MY INVITATION 4\n\
        \CONSIDER THAT A DIVORCE b\n" `shouldBe`
        Just
          (R.At
             (R.Region
                { R.start = R.Position {R._line = 2, R._column = 1}
                , R.end = R.Position {R._line = 2, R._column = 25}
                })
             (BinaryOp
                Or
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 1, R._column = 23}
                      , R.end = R.Position {R._line = 1, R._column = 24}
                      })
                   (Int 4))
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 2, R._column = 25}
                      , R.end = R.Position {R._line = 2, R._column = 26}
                      })
                   (Var "b"))))
    it "should parse 'and' operation" $ do
      parseMaybe
        expressionParser
        "HERE IS MY INVITATION 4\n\
        \KNOCK KNOCK b\n" `shouldBe`
        Just
          (R.At
             (R.Region
                { R.start = R.Position {R._line = 2, R._column = 1}
                , R.end = R.Position {R._line = 2, R._column = 13}
                })
             (BinaryOp
                And
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 1, R._column = 23}
                      , R.end = R.Position {R._line = 1, R._column = 24}
                      })
                   (Int 4))
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 2, R._column = 13}
                      , R.end = R.Position {R._line = 2, R._column = 14}
                      })
                   (Var "b"))))
    it "should parse 'equal to' operation" $ do
      parseMaybe
        expressionParser
        "HERE IS MY INVITATION 4\n\
        \YOU ARE NOT YOU YOU ARE ME b\n" `shouldBe`
        Just
          (R.At
             (R.Region
                { R.start = R.Position {R._line = 2, R._column = 1}
                , R.end = R.Position {R._line = 2, R._column = 28}
                })
             (BinaryOp
                EqualTo
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 1, R._column = 23}
                      , R.end = R.Position {R._line = 1, R._column = 24}
                      })
                   (Int 4))
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 2, R._column = 28}
                      , R.end = R.Position {R._line = 2, R._column = 29}
                      })
                   (Var "b"))))
    it "should parse 'equal to' operation" $ do
      parseMaybe
        expressionParser
        "HERE IS MY INVITATION 4\n\
        \LET OFF SOME STEAM BENNET b\n" `shouldBe`
        Just
          (R.At
             (R.Region
                { R.start = R.Position {R._line = 2, R._column = 1}
                , R.end = R.Position {R._line = 2, R._column = 27}
                })
             (BinaryOp
                GreaterThan
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 1, R._column = 23}
                      , R.end = R.Position {R._line = 1, R._column = 24}
                      })
                   (Int 4))
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 2, R._column = 27}
                      , R.end = R.Position {R._line = 2, R._column = 28}
                      })
                   (Var "b"))))
    it "should parse chained binary operations" $
      -- (((4 + b) > 3) && 1) || 0
     do
      parseMaybe
        expressionParser
        "HERE IS MY INVITATION 4\n\
        \GET UP b\n\
        \LET OFF SOME STEAM BENNET 3\n\
        \KNOCK KNOCK @NO PROBLEMO\n\
        \CONSIDER THAT A DIVORCE @I LIED\n" `shouldBe`
        Just
          (R.At
             (R.Region
                { R.start = R.Position {R._line = 5, R._column = 1}
                , R.end = R.Position {R._line = 5, R._column = 25}
                })
             (BinaryOp
                Or
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 4, R._column = 1}
                      , R.end = R.Position {R._line = 4, R._column = 13}
                      })
                   (BinaryOp
                      And
                      (R.At
                         (R.Region
                            { R.start = R.Position {R._line = 3, R._column = 1}
                            , R.end = R.Position {R._line = 3, R._column = 27}
                            })
                         (BinaryOp
                            GreaterThan
                            (R.At
                               (R.Region
                                  { R.start =
                                      R.Position {R._line = 2, R._column = 1}
                                  , R.end =
                                      R.Position {R._line = 2, R._column = 8}
                                  })
                               (BinaryOp
                                  Add
                                  (R.At
                                     (R.Region
                                        { R.start =
                                            R.Position
                                              {R._line = 1, R._column = 23}
                                        , R.end =
                                            R.Position
                                              {R._line = 1, R._column = 24}
                                        })
                                     (Int 4))
                                  (R.At
                                     (R.Region
                                        { R.start =
                                            R.Position
                                              {R._line = 2, R._column = 8}
                                        , R.end =
                                            R.Position
                                              {R._line = 2, R._column = 9}
                                        })
                                     (Var "b"))))
                            (R.At
                               (R.Region
                                  { R.start =
                                      R.Position {R._line = 3, R._column = 27}
                                  , R.end =
                                      R.Position {R._line = 3, R._column = 28}
                                  })
                               (Int 3))))
                      (R.At
                         (R.Region
                            { R.start = R.Position {R._line = 4, R._column = 13}
                            , R.end = R.Position {R._line = 4, R._column = 25}
                            })
                         (Int 1))))
                (R.At
                   (R.Region
                      { R.start = R.Position {R._line = 5, R._column = 25}
                      , R.end = R.Position {R._line = 5, R._column = 32}
                      })
                   (Int 0))))
