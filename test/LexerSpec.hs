module LexerSpec where

import AST
import Lexer
import Test.Hspec
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
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
              opStrings = map snd ops
      let astResults =
            map (\astOp -> Just (BinaryOp astOp (Int 4) (Var "b"))) astOps
            where
              astOps = map fst ops
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
                (BinaryOp GreaterThan (BinaryOp Add (Int 4) (Var "b")) (Int 3))
                (Int 1))
             (Int 0))
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
        Just (Assignment "myvar" (Int 4))
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
                   (BinaryOp Mult (BinaryOp Add (Int 4) (Var "b")) (Int 5))
                   (Int 1))
                (Var "send")))
  describe "print statement parser" $ do
    it "should parse double-quoted strings" $ do
      parseMaybe printStatementParser "TALK TO THE HAND \"Hello\"" `shouldBe`
        Just (Print (String "Hello"))
    it "should not parse single-quoted strings" $ do
      parseMaybe printStatementParser "TALK TO THE HAND \'Hello\'" `shouldBe`
        Nothing
    it "should parse printing of an integer" $ do
      parseMaybe printStatementParser "TALK TO THE HAND 4" `shouldBe`
        Just (Print (Int 4))
    it "should parse a variable reference" $ do
      parseMaybe printStatementParser "TALK TO THE HAND a" `shouldBe`
        Just (Print (Var "a"))
  describe "int declaration parser" $ do
    it "should parse integer" $ do
      parseMaybe intDeclarationStatementParser "HEY CHRISTMAS TREE 5" `shouldBe`
        Just (IntVar 5)
    it "should not parse a string" $ do
      parseMaybe intDeclarationStatementParser "HEY CHRISTMAS TREE \"hola\"" `shouldBe`
        Nothing
    it "should not parse a variable" $ do
      parseMaybe intDeclarationStatementParser "HEY CHRISTMAS TREE a" `shouldBe`
        Nothing
  describe "if-else parser" $ do
    it "should parse if statements without else" $ do
      parseMaybe
        ifStatementParser
        "BECAUSE I'M GOING TO SAY PLEASE a \
        \TALK TO THE HAND \"a is true\" \
        \TALK TO THE HAND \"a' is unknown\" \
        \YOU HAVE NO RESPECT FOR LOGIC" `shouldBe`
        Just
          (If
             (Var "a")
             [Print (String "a is true"), Print (String "a' is unknown")]
             [])
    it "should parse if statements with else" $ do
      parseMaybe
        ifStatementParser
        "BECAUSE I'M GOING TO SAY PLEASE a \
        \TALK TO THE HAND \"a is true\" \
        \BULLSHIT \
        \TALK TO THE HAND \"a is not true\" \
        \TALK TO THE HAND \"a' might be\" \
        \YOU HAVE NO RESPECT FOR LOGIC" `shouldBe`
        Just
          (If
             (Var "a")
             [Print (String "a is true")]
             [Print (String "a is not true"), Print (String "a' might be")])
    it "should parse empty if and else statements" $ do
      parseMaybe
        ifStatementParser
        "BECAUSE I'M GOING TO SAY PLEASE a \
        \BULLSHIT \
        \YOU HAVE NO RESPECT FOR LOGIC" `shouldBe`
        Just (If (Var "a") [] [])
  describe "while parser" $ do
    it "should parse while statements" $ do
      parseMaybe
        whileStatementParser
        "STICK AROUND a \
        \TALK TO THE HAND \"a is true\" \
        \CHILL" `shouldBe`
        Just (While (Var "a") [Print (String "a is true")])
  describe "method parser" $ do
    it "should parse empty method" $ do
      parseMaybe
        methodParser
        "LISTEN TO ME VERY CAREFULLY aMethod \
        \HASTA LA VISTA, BABY" `shouldBe`
        Just (Method "aMethod" [] [])
    it "should parse non-void method with 2 arguments" $ do
      parseMaybe
        methodParser
        "LISTEN TO ME VERY CAREFULLY aMethod \
        \I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg1 \
        \I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg2 \
        \GIVE THESE PEOPLE AIR \
        \GET TO THE CHOPPER myvar \
        \HERE IS MY INVITATION 4 \
        \GET UP b \
        \YOU'RE FIRED 5 \
        \GET DOWN 1 \
        \HE HAD TO SPLIT send \
        \ENOUGH TALK \
        \HASTA LA VISTA, BABY" `shouldBe`
        Just
          (Method
             "aMethod"
             [MethodArg "arg1", MethodArg "arg2"]
             [ (Assignment
                  "myvar"
                  (BinaryOp
                     Divide
                     (BinaryOp
                        Minus
                        (BinaryOp Mult (BinaryOp Add (Int 4) (Var "b")) (Int 5))
                        (Int 1))
                     (Var "send")))
             ])
  describe "program parser" $ do
    it "should parse main and all other methods" $ do
      parseMaybe
        programParser
        "LISTEN TO ME VERY CAREFULLY aMethod \
        \HASTA LA VISTA, BABY \
        \IT'S SHOWTIME \
        \YOU HAVE BEEN TERMINATED \
        \\
        \LISTEN TO ME VERY CAREFULLY aMethod2 \
        \I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg1 \
        \I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg2 \
        \HASTA LA VISTA, BABY" `shouldBe`
        Just
          [ Method "aMethod" [] []
          , Main []
          , Method "aMethod2" [MethodArg "arg1", MethodArg "arg2"] []
          ]
