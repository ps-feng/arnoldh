module Parser.MethodSpec where

import AST
import Parser
import qualified Region as R
import Test.Hspec
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
  describe "method parser" $ do
    it "should parse empty method" $ do
      parseMaybe
        methodParser
        "LISTEN TO ME VERY CAREFULLY aMethod\n\
        \HASTA LA VISTA, BABY\n" `shouldBe`
        Just
          (Method
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 1, R._column = 29}
                   , R.end = R.Position {R._line = 1, R._column = 36}
                   })
                "aMethod")
             []
             [])
    it "should parse non-void method with 2 arguments" $ do
      parseMaybe
        methodParser
        "LISTEN TO ME VERY CAREFULLY aMethod\n\
        \I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg1\n\
        \I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg2\n\
        \GIVE THESE PEOPLE AIR\n\
        \GET TO THE CHOPPER myvar\n\
        \HERE IS MY INVITATION 4\n\
        \GET UP b\n\
        \YOU'RE FIRED 5\n\
        \GET DOWN 1\n\
        \HE HAD TO SPLIT send\n\
        \ENOUGH TALK\n\
        \HASTA LA VISTA, BABY\n" `shouldBe`
        Just
          (Method
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 1, R._column = 29}
                   , R.end = R.Position {R._line = 1, R._column = 36}
                   })
                "aMethod")
             [ R.At
                 (R.Region
                    { R.start = R.Position {R._line = 2, R._column = 52}
                    , R.end = R.Position {R._line = 2, R._column = 56}
                    })
                 (Var "arg1")
             , R.At
                 (R.Region
                    { R.start = R.Position {R._line = 3, R._column = 52}
                    , R.end = R.Position {R._line = 3, R._column = 56}
                    })
                 (Var "arg2")
             ]
             [ R.At
                 (R.Region
                    { R.start = R.Position {R._line = 5, R._column = 1}
                    , R.end = R.Position {R._line = 12, R._column = 1}
                    })
                 (Assignment
                    (R.At
                       (R.Region
                          { R.start = R.Position {R._line = 5, R._column = 20}
                          , R.end = R.Position {R._line = 5, R._column = 25}
                          })
                       "myvar")
                    (R.At
                       (R.Region
                          { R.start = R.Position {R._line = 10, R._column = 1}
                          , R.end = R.Position {R._line = 10, R._column = 17}
                          })
                       (BinaryOp
                          Divide
                          (R.At
                             (R.Region
                                { R.start = R.Position {R._line = 9, R._column = 1}
                                , R.end = R.Position {R._line = 9, R._column = 10}
                                })
                             (BinaryOp
                                Minus
                                (R.At
                                   (R.Region
                                      { R.start =
                                          R.Position {R._line = 8, R._column = 1}
                                      , R.end = R.Position {R._line = 8, R._column = 14}
                                      })
                                   (BinaryOp
                                      Mult
                                      (R.At
                                         (R.Region
                                            { R.start =
                                                R.Position
                                                  {R._line = 7, R._column = 1}
                                            , R.end =
                                                R.Position
                                                  {R._line = 7, R._column = 8}
                                            })
                                         (BinaryOp
                                            Add
                                            (R.At
                                               (R.Region
                                                  { R.start =
                                                      R.Position
                                                        { R._line = 6
                                                        , R._column = 23
                                                        }
                                                  , R.end =
                                                      R.Position
                                                        { R._line = 6
                                                        , R._column = 24
                                                        }
                                                  })
                                               (Int 4))
                                            (R.At
                                               (R.Region
                                                  { R.start =
                                                      R.Position
                                                        {R._line = 7, R._column = 8}
                                                  , R.end =
                                                      R.Position
                                                        {R._line = 7, R._column = 9}
                                                  })
                                               (Var "b"))))
                                      (R.At
                                         (R.Region
                                            { R.start =
                                                R.Position
                                                  {R._line = 8, R._column = 14}
                                            , R.end =
                                                R.Position
                                                  {R._line = 8, R._column = 15}
                                            })
                                         (Int 5))))
                                (R.At
                                   (R.Region
                                      { R.start =
                                          R.Position {R._line = 9, R._column = 10}
                                      , R.end = R.Position {R._line = 9, R._column = 11}
                                      })
                                   (Int 1))))
                          (R.At
                             (R.Region
                                { R.start = R.Position {R._line = 10, R._column = 17}
                                , R.end = R.Position {R._line = 10, R._column = 21}
                                })
                             (Var "send")))))
             ])
    it "should parse method with empty return statement" $ do
      parseMaybe
        methodParser
        "LISTEN TO ME VERY CAREFULLY aMethod\n\
        \GIVE THESE PEOPLE AIR\n\
        \I'LL BE BACK  \n  \
        \HASTA LA VISTA, BABY\n" `shouldBe`
        Just
          (Method
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 1, R._column = 29}
                   , R.end = R.Position {R._line = 1, R._column = 36}
                   })
                "aMethod")
             []
             [ R.At
                 (R.Region
                    { R.start = R.Position {R._line = 3, R._column = 1}
                    , R.end = R.Position {R._line = 4, R._column = 3}
                    })
                 (Return Nothing)
             ])
    it "should parse method with return statement with literal" $ do
      parseMaybe
        methodParser
        "LISTEN TO ME VERY CAREFULLY aMethod\n\
        \GIVE THESE PEOPLE AIR\n\
        \I'LL BE BACK 4\n\
        \HASTA LA VISTA, BABY\n" `shouldBe`
        Just
          (Method
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 1, R._column = 29}
                   , R.end = R.Position {R._line = 1, R._column = 36}
                   })
                "aMethod")
             []
             [ R.At
                 (R.Region
                    { R.start = R.Position {R._line = 3, R._column = 1}
                    , R.end = R.Position {R._line = 4, R._column = 1}
                    })
                 (Return
                    (Just
                       (R.At
                          (R.Region
                             { R.start = R.Position {R._line = 3, R._column = 14}
                             , R.end = R.Position {R._line = 3, R._column = 15}
                             })
                          (Int 4))))
             ])
