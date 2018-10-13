module Parser.AssignmentSpec where

import AST
import Parser
import qualified Region as R
import Test.Hspec
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
  describe "assignment parser" $ do
    it "should fail if assignment statements are not provided" $ do
      parseMaybe
        assignmentParser
        "GET TO THE CHOPPER myvar\n\
        \ENOUGH TALK\n" `shouldBe`
        Nothing
    it "should parse a single integer assignment" $ do
      parseMaybe
        assignmentParser
        "GET TO THE CHOPPER myvar\n\
        \HERE IS MY INVITATION 4\n\
        \ENOUGH TALK\n" `shouldBe`
        Just
          (Assignment
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 1, R._column = 20}
                   , R.end = R.Position {R._line = 1, R._column = 25}
                   })
                "myvar")
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 2, R._column = 23}
                   , R.end = R.Position {R._line = 2, R._column = 24}
                   })
                (Int 4)))
    it "should parse a variable assignment" $ do
      parseMaybe
        assignmentParser
        "GET TO THE CHOPPER myvar\n\
        \HERE IS MY INVITATION a\n\
        \ENOUGH TALK\n" `shouldBe`
        Just
          (Assignment
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 1, R._column = 20}
                   , R.end = R.Position {R._line = 1, R._column = 25}
                   })
                "myvar")
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 2, R._column = 23}
                   , R.end = R.Position {R._line = 2, R._column = 24}
                   })
                (Var "a")))
    it "should parse a list of operations" $ do
      parseMaybe
        assignmentParser
        "GET TO THE CHOPPER myvar\n\
        \HERE IS MY INVITATION 4\n\
        \GET UP b\n\
        \YOU'RE FIRED 5\n\
        \GET DOWN 1\n\
        \HE HAD TO SPLIT send\n\
        \ENOUGH TALK\n" `shouldBe`
        Just
          (Assignment
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 1, R._column = 20}
                   , R.end = R.Position {R._line = 1, R._column = 25}
                   })
                "myvar")
             (R.At
                (R.Region
                   { R.start = R.Position {R._line = 6, R._column = 1}
                   , R.end = R.Position {R._line = 6, R._column = 17}
                   })
                (BinaryOp
                   Divide
                   (R.At
                      (R.Region
                         { R.start = R.Position {R._line = 5, R._column = 1}
                         , R.end = R.Position {R._line = 5, R._column = 10}
                         })
                      (BinaryOp
                         Minus
                         (R.At
                            (R.Region
                               { R.start = R.Position {R._line = 4, R._column = 1}
                               , R.end = R.Position {R._line = 4, R._column = 14}
                               })
                            (BinaryOp
                               Mult
                               (R.At
                                  (R.Region
                                     { R.start = R.Position {R._line = 3, R._column = 1}
                                     , R.end = R.Position {R._line = 3, R._column = 8}
                                     })
                                  (BinaryOp
                                     Add
                                     (R.At
                                        (R.Region
                                           { R.start =
                                               R.Position
                                                 {R._line = 2, R._column = 23}
                                           , R.end =
                                               R.Position
                                                 {R._line = 2, R._column = 24}
                                           })
                                        (Int 4))
                                     (R.At
                                        (R.Region
                                           { R.start =
                                               R.Position {R._line = 3, R._column = 8}
                                           , R.end =
                                               R.Position {R._line = 3, R._column = 9}
                                           })
                                        (Var "b"))))
                               (R.At
                                  (R.Region
                                     { R.start =
                                         R.Position {R._line = 4, R._column = 14}
                                     , R.end = R.Position {R._line = 4, R._column = 15}
                                     })
                                  (Int 5))))
                         (R.At
                            (R.Region
                               { R.start = R.Position {R._line = 5, R._column = 10}
                               , R.end = R.Position {R._line = 5, R._column = 11}
                               })
                            (Int 1))))
                   (R.At
                      (R.Region
                         { R.start = R.Position {R._line = 6, R._column = 17}
                         , R.end = R.Position {R._line = 6, R._column = 21}
                         })
                      (Var "send")))))
