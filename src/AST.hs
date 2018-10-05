module AST where

type Program = [AbstractMethod]

data Op
  = Add
  | Minus
  | Mult
  | Divide
  | Modulo
  | Or
  | And
  | EqualTo
  | GreaterThan
  deriving (Show, Eq)

data Expr
  = Int Integer
  | String String -- check also Data.Text
  | Var String
  | BinaryOp Op
             Expr
             Expr
  deriving (Show, Eq)

data Statement
  = Assignment String
               Expr
  | Print Expr
  | IntVar Integer -- TODO: 16bit signed
  | If Expr
       [Statement]
       [Statement]
  | While Expr
          [Statement]
  deriving (Show, Eq)

data MethodArg
  = MethodArg String
  deriving (Show, Eq)

data AbstractMethod
  = Main [Statement]
  | Method [MethodArg] [Statement]
  deriving (Show, Eq)
--
-- GRAMMAR DEFINITION
--
-- also in: https://github.com/jroweboy/ArnoldC.js/blob/master/src/main/arnoldc.pegjs
--
-- program ::=
--   "IT'S SHOWTIME"
--   statement*
--   "YOU HAVE BEEN TERMINATED"
--
-- statement ::=
--     print |
--     declaration |
--     readInteger |
--     if |
--     while |
--     methodDeclaration |
--     methodInvocation
--
-- print ::= "TALK TO THE HAND" term
-- 
-- declaration ::= declarationIdentifier declarationInitialValue
-- declarationIdentifier ::= "HEY CHRISTMAS TREE" identifier
-- declarationInitialValue ::= "YOU SET US UP" term
--
-- readInteger ::= "I WANT TO ASK YOU A BUNCH OF QUESTIONS AND I WANT TO HAVE THEM ANSWERED IMMEDIATELY" 
-- 
-- assignment ::= assignmentIdentifier setInitialValue expression* assignmentEnd
-- assignmentIdentifier ::= "GET TO THE CHOPPER" identifier
-- assignmentInitialValue ::= "HERE IS MY INVITATION" term
-- assignmentEnd ::= "ENOUGH TALK"
-- 
-- expression ::= 
--     arithmeticExpression |
--     logicalExpression
-- 
-- arithmeticExpression ::= 
--     "GET UP" arithmeticalTerm | 
--     "GET DOWN" arithmeticalTerm | 
--     "YOU'RE FIRED" arithmeticalTerm | 
--     "HE HAD TO SPLIT" arithmeticalTerm |
--     "I LET HIM GO" arithmeticalTerm
-- 
-- logicalExpression ::=
--     "YOU ARE NOT YOU YOU ARE ME" term |
--     "LET OFF SOME STEAM BENNET" arithmeticalTerm |
--     "CONSIDER THAT A DIVORCE" booleanTerm |
--     "KNOCK KNOCK" booleanTerm
-- 
-- if ::= "BECAUSE I'M GOING TO SAY PLEASE" term statement* else? "YOU HAVE NO RESPECT FOR LOGIC"
-- else ::= "BULLSHIT" statement*
--
-- while ::= "STICK AROUND" booleanTerm statement* "CHILL"
--
-- methodDeclaration ::= "LISTEN TO ME VERY CAREFULLY" identifier methodDeclarationArguments? statement* methodReturn? "HASTA LA VISTA, BABY"
-- methodDeclarationArguments ::= methodDeclarationArgument+ "GIVE THESE PEOPLE AIR"
-- methodDeclarationArgument ::= "I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE" identifier
--
-- methodInvocation ::= "DO IT NOW" identifier? term?
--
-- arithmeticalTerm ::= identifier | number
-- booleanTerm ::= identifier | boolean
-- term ::= string | identifier | number | boolean
--
-- string ::= ".*"
-- identifier ::= [A-Za-z]+
-- number := [0-9]+
-- boolean := "@I LIED" | "@NO PROBLEMO"
