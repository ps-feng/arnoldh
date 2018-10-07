module Lexer where

import AST
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

arnParseError = "WHAT THE FUCK DID I DO WRONG"

arnDeclareInt = "HEY CHRISTMAS TREE"

arnSetInitialValue = "YOU SET US UP"

arnBeginMain = "IT'S SHOWTIME"

arnEndMain = "YOU HAVE BEEN TERMINATED"

arnPlusOperator = "GET UP"

arnMinusOperator = "GET DOWN"

arnMultiplicationOperator = "YOU'RE FIRED"

arnDivisionOperator = "HE HAD TO SPLIT"

arnModulo = "I LET HIM GO"

arnPrint = "TALK TO THE HAND"

arnRead =
  "I WANT TO ASK YOU A BUNCH OF QUESTIONS AND I WANT TO HAVE THEM ANSWERED IMMEDIATELY"

arnAssignVariable = "GET TO THE CHOPPER"

arnSetValue = "HERE IS MY INVITATION"

arnEndAssignVariable = "ENOUGH TALK"

arnFalse = "@I LIED"

arnTrue = "@NO PROBLEMO"

arnEqualTo = "YOU ARE NOT YOU YOU ARE ME"

arnGreaterThan = "LET OFF SOME STEAM BENNET"

arnOr = "CONSIDER THAT A DIVORCE"

arnAnd = "KNOCK KNOCK"

arnIf = "BECAUSE I'M GOING TO SAY PLEASE"

arnElse = "BULLSHIT"

arnEndIf = "YOU HAVE NO RESPECT FOR LOGIC"

arnWhile = "STICK AROUND"

arnEndWhile = "CHILL"

arnDeclareMethod = "LISTEN TO ME VERY CAREFULLY"

arnMethodArguments = "I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE"

arnReturn = "I'LL BE BACK"

arnEndMethodDeclaration = "HASTA LA VISTA, BABY"

arnCallMethod = "DO IT NOW"

arnNonVoidMethod = "GIVE THESE PEOPLE AIR"

arnAssignVariableFromMethodCall = "GET YOUR ASS TO MARS"

-- Useful links
-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
-- http://akashagrawal.me/beginners-guide-to-megaparsec/
type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = space

-- this will trim all the whitespace after consuming the parsed lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

beginMainParser :: Parser String
beginMainParser = lexeme (string arnBeginMain)

endMainParser :: Parser String
endMainParser = lexeme (string arnEndMain)

setValueParser :: Parser String
setValueParser = lexeme (string arnSetValue)

integerParser :: Parser Integer
integerParser = lexeme L.decimal

stringParser :: Parser String
stringParser = lexeme (char '"' >> manyTill L.charLiteral (char '"'))

booleanWords :: [String]
booleanWords = [arnTrue, arnFalse]

booleanParser :: Parser String
booleanParser = foldr1 (<|>) (map (lexeme . try . string) booleanWords)

startAssignParser :: Parser String
startAssignParser = lexeme (string arnAssignVariable)

endAssignParser :: Parser String
endAssignParser = lexeme (string arnEndAssignVariable)

binaryOperators :: [String]
binaryOperators =
  [ arnPlusOperator
  , arnMinusOperator
  , arnDivisionOperator
  , arnMultiplicationOperator
  , arnModulo
  -- logical operators
  , arnOr
  , arnAnd
  -- comparison
  , arnEqualTo
  , arnGreaterThan
  ]

binaryOperatorParser :: Parser String
binaryOperatorParser =
  foldr1 (<|>) (map (lexeme . try . string) binaryOperators)

reservedWords :: [String]
reservedWords =
  binaryOperators ++
  [arnSetValue, arnPrint, arnIf, arnElse, arnEndIf, arnWhile, arnEndWhile]

reservedWordParser :: String -> Parser ()
reservedWordParser word =
  (lexeme . try) (string word *> notFollowedBy alphaNumChar)

identifierParser :: Parser String
identifierParser = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

printParser :: Parser String
printParser = lexeme (string arnPrint)

intDeclarationParser :: Parser String
intDeclarationParser = lexeme (string arnDeclareInt)

ifParser :: Parser String
ifParser = lexeme (string arnIf)

elseParser :: Parser String
elseParser = lexeme (string arnElse)

endIfParser :: Parser String
endIfParser = lexeme (string arnEndIf)

whileParser :: Parser String
whileParser = lexeme (string arnWhile)

endWhileParser :: Parser String
endWhileParser = lexeme (string arnEndWhile)

--- Parser after this
intToIntConst :: Parser Integer -> Parser Expr
intToIntConst x = x >>= \x' -> return (Int x')

booleanToIntConst :: Parser String -> Parser Expr
booleanToIntConst x =
  x >>=
  (\x' ->
     case () of
       _
         | x' == arnFalse -> return (Int 0)
         | x' == arnTrue -> return (Int 1)
         | otherwise -> fail "invalid value")

stringToStringConst :: Parser String -> Parser Expr
stringToStringConst x = x >>= \x' -> return (String x')

identifierToVarParser :: Parser String -> Parser Expr
identifierToVarParser x = x >>= (\x' -> return (Var x'))

binaryOpTermParser :: Parser Expr
binaryOpTermParser =
  intToIntConst integerParser <|> booleanTermParser <|>
  identifierToVarParser identifierParser

booleanTermParser :: Parser Expr
booleanTermParser = booleanToIntConst booleanParser

ops :: [(Op, String)]
ops =
  [ (Add, "GET UP")
  , (Minus, "GET DOWN")
  , (Divide, "HE HAD TO SPLIT")
  , (Mult, "YOU'RE FIRED")
  , (Modulo, "I LET HIM GO")
  -- logical operators
  , (Or, "CONSIDER THAT A DIVORCE")
  , (And, "KNOCK KNOCK")
  -- comparison
  , (EqualTo, "YOU ARE NOT YOU YOU ARE ME")
  , (GreaterThan, "LET OFF SOME STEAM BENNET")
  ]

operatorTable :: [[Operator Parser Expr]]
operatorTable = [map (\(op, sym) -> InfixL (BinaryOp op <$ symbol sym)) ops]

expressionParser :: Parser Expr
expressionParser = do
  setValueParser
  makeExprParser binaryOpTermParser operatorTable

assignmentParser :: Parser Statement
assignmentParser = do
  startAssignParser
  id <- identifierParser
  expr <- expressionParser
  endAssignParser
  return (Assignment id expr)

termParser :: Parser Expr
termParser =
  (intToIntConst integerParser) <|> (identifierToVarParser identifierParser) <|>
  (stringToStringConst stringParser)

printStatementParser :: Parser Statement
printStatementParser = do
  printParser
  term <- termParser
  return (Print term)

intDeclarationStatementParser :: Parser Statement
intDeclarationStatementParser = do
  intDeclarationParser
  number <- integerParser
  return (IntVar number)

elseStatementParser :: Parser [Statement]
elseStatementParser = do
  elseParser
  elseStatements <- many statementParser
  return elseStatements

ifStatementParser :: Parser Statement
ifStatementParser = do
  ifParser
  condition <- termParser
  ifStatements <- many statementParser
  elseStatements <- try elseStatementParser <|> (return [])
  endIfParser
  return (If condition ifStatements elseStatements)

whileStatementParser :: Parser Statement
whileStatementParser = do
  whileParser
  condition <- termParser
  statements <- many statementParser
  endWhileParser
  return (While condition statements)

-- call method statement, return statement, call read statement
statementParser :: Parser Statement
statementParser =
  assignmentParser <|> printStatementParser <|> intDeclarationStatementParser <|>
  ifStatementParser <|>
  whileStatementParser

mainMethodParser :: Parser AbstractMethod
mainMethodParser = do
  beginMainParser
  statements <- many statementParser
  endMainParser
  return (Main statements)

argumentParser :: Parser MethodArg
argumentParser = do
  reservedWordParser arnMethodArguments
  argument <- identifierParser
  return (MethodArg argument)

methodStatementsParser :: Parser [Statement]
methodStatementsParser = do
  reservedWordParser arnNonVoidMethod
  statements <- many statementParser
  return statements

methodParser :: Parser AbstractMethod
methodParser = do
  reservedWordParser arnDeclareMethod
  name <- identifierParser
  arguments <- many argumentParser
  statements <- try methodStatementsParser <|> (return [])
  reservedWordParser arnEndMethodDeclaration
  return (Method name arguments statements)

abstractMethodParser :: Parser AbstractMethod
abstractMethodParser = mainMethodParser <|> methodParser

programParser :: Parser Program
programParser = do
  methods <- some abstractMethodParser
  return methods
