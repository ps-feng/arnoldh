module ParserSpec where

import AST
import Parser
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
                   ("HERE IS MY INVITATION 4\n" ++ opStr ++ " b\n"))
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
        "HERE IS MY INVITATION 4\n\
        \GET UP b\n\
        \LET OFF SOME STEAM BENNET 3\n\
        \KNOCK KNOCK @NO PROBLEMO\n\
        \CONSIDER THAT A DIVORCE @I LIED\n" `shouldBe`
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
        "GET TO THE CHOPPER myvar\n\
        \ENOUGH TALK\n" `shouldBe`
        Nothing
    it "should parse a single integer assignment" $ do
      parseMaybe
        assignmentParser
        "GET TO THE CHOPPER myvar\n\
        \HERE IS MY INVITATION 4\n\
        \ENOUGH TALK\n" `shouldBe`
        Just (Assignment "myvar" (Int 4))
    it "should parse a variable assignment" $ do
      parseMaybe
        assignmentParser
        "GET TO THE CHOPPER myvar\n\
        \HERE IS MY INVITATION a\n\
        \ENOUGH TALK\n" `shouldBe`
        Just (Assignment "myvar" (Var "a"))
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
      parseMaybe printStatementParser "TALK TO THE HAND \"Hello\"\n" `shouldBe`
        Just (Print (String "Hello"))
    it "should not parse single-quoted strings" $ do
      parseMaybe printStatementParser "TALK TO THE HAND \'Hello\'\n" `shouldBe`
        Nothing
    it "should parse printing of an integer" $ do
      parseMaybe printStatementParser "TALK TO THE HAND 4\n" `shouldBe`
        Just (Print (Int 4))
    it "should parse a variable reference" $ do
      parseMaybe printStatementParser "TALK TO THE HAND a\n" `shouldBe`
        Just (Print (Var "a"))
  describe "int declaration parser" $ do
    it "should parse integer" $ do
      parseMaybe intDeclarationStatementParser "HEY CHRISTMAS TREE 5\n" `shouldBe`
        Just (IntVar 5)
    it "should not parse a string" $ do
      parseMaybe intDeclarationStatementParser "HEY CHRISTMAS TREE \"hola\"\n" `shouldBe`
        Nothing
    it "should not parse a variable" $ do
      parseMaybe intDeclarationStatementParser "HEY CHRISTMAS TREE a\n" `shouldBe`
        Nothing
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
             (Var "a")
             [Print (String "a is true"), Print (String "a' is unknown")]
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
             (Var "a")
             [Print (String "a is true")]
             [Print (String "a is not true"), Print (String "a' might be")])
    it "should parse empty if and else statements" $ do
      parseMaybe
        ifStatementParser
        "BECAUSE I'M GOING TO SAY PLEASE a\n\
        \BULLSHIT\n\
        \YOU HAVE NO RESPECT FOR LOGIC\n" `shouldBe`
        Just (If (Var "a") [] [])
  describe "while parser" $ do
    it "should parse while statements" $ do
      parseMaybe
        whileStatementParser
        "STICK AROUND a\n\
        \TALK TO THE HAND \"a is true\"\n\
        \CHILL" `shouldBe`
        Just (While (Var "a") [Print (String "a is true")])
  describe "method parser" $ do
    it "should parse empty method" $ do
      parseMaybe
        methodParser
        "LISTEN TO ME VERY CAREFULLY aMethod\n\
        \HASTA LA VISTA, BABY\n" `shouldBe`
        Just (Method "aMethod" [] [])
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
    it "should parse method with empty return statement" $ do
      parseMaybe
        methodParser
        "LISTEN TO ME VERY CAREFULLY aMethod\n\
        \GIVE THESE PEOPLE AIR\n\
        \I'LL BE BACK  \n  \
        \HASTA LA VISTA, BABY\n" `shouldBe`
        Just (Method "aMethod" [] [(Return Nothing)])
    it "should parse method with return statement with literal" $ do
      parseMaybe
        methodParser
        "LISTEN TO ME VERY CAREFULLY aMethod\n\
        \GIVE THESE PEOPLE AIR\n\
        \I'LL BE BACK 4\n\
        \HASTA LA VISTA, BABY\n" `shouldBe`
        Just (Method "aMethod" [] [(Return (Just (Int 4)))])
  describe "call read method parser" $ do
    it "should parse it correctly" $ do
      parseMaybe
        callReadMethodStatementParser
        "GET YOUR ASS TO MARS input\n\
        \DO IT NOW\n\
        \I WANT TO ASK YOU A BUNCH OF QUESTIONS AND I WANT TO HAVE THEM ANSWERED IMMEDIATELY\n" `shouldBe`
        Just (CallRead "input")
  describe "program parser" $ do
    it "should parse main and all other methods" $ do
      parseMaybe
        programParser
        "LISTEN TO ME VERY CAREFULLY aMethod\n\
        \HASTA LA VISTA, BABY\n\
        \    \
        \IT'S SHOWTIME\n\
        \YOU HAVE BEEN TERMINATED\n\
        \\
        \LISTEN TO ME VERY CAREFULLY aMethod2\n\
        \I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg1\n\
        \I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg2\n\
        \HASTA LA VISTA, BABY\n" `shouldBe`
        Just
          [ Method "aMethod" [] []
          , Main []
          , Method "aMethod2" [MethodArg "arg1", MethodArg "arg2"] []
          ]
