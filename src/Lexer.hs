module Lexer where

import AST
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

arnParseError = "WHAT THE FUCK DID I DO WRONG"

arnRead =
  "I WANT TO ASK YOU A BUNCH OF QUESTIONS AND I WANT TO HAVE THEM ANSWERED IMMEDIATELY"

arnReturn = "I'LL BE BACK"

arnCallMethod = "DO IT NOW"

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

integerParser :: Parser Integer
integerParser = lexeme L.decimal

stringLiteralParser :: Parser String
stringLiteralParser = lexeme (char '"' >> manyTill L.charLiteral (char '"'))

reservedWordParser :: String -> Parser ()
reservedWordParser word =
  (lexeme . try) (string word *> notFollowedBy alphaNumChar)

identifierParser :: Parser String
identifierParser = (lexeme . try) p
  where
    p = (:) <$> letterChar <*> many alphaNumChar

binaryOpTermParser :: Parser Expr
binaryOpTermParser =
  Int <$> integerParser <|> booleanTermParser <|> Var <$> identifierParser

booleanTermParser :: Parser Expr
booleanTermParser =
  (Int 0 <$ symbol "@I LIED") <|> (Int 1 <$ symbol "@NO PROBLEMO")

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
  symbol "HERE IS MY INVITATION"
  makeExprParser binaryOpTermParser operatorTable

assignmentParser :: Parser Statement
assignmentParser = do
  symbol "GET TO THE CHOPPER"
  id <- identifierParser
  expr <- expressionParser
  symbol "ENOUGH TALK"
  return (Assignment id expr)

termParser :: Parser Expr
termParser =
  Int <$> integerParser <|> Var <$> identifierParser <|>
  String <$> stringLiteralParser

printStatementParser :: Parser Statement
printStatementParser = do
  symbol "TALK TO THE HAND"
  term <- termParser
  return (Print term)

intDeclarationStatementParser :: Parser Statement
intDeclarationStatementParser = do
  symbol "HEY CHRISTMAS TREE"
  number <- integerParser
  return (IntVar number)

ifStatementParser :: Parser Statement
ifStatementParser = do
  symbol "BECAUSE I'M GOING TO SAY PLEASE"
  condition <- termParser
  ifStatements <- many statementParser
  elseStatements <- try elseStatementParser <|> (return [])
  symbol "YOU HAVE NO RESPECT FOR LOGIC"
  return (If condition ifStatements elseStatements)

elseStatementParser :: Parser [Statement]
elseStatementParser = do
  symbol "BULLSHIT"
  elseStatements <- many statementParser
  return elseStatements

whileStatementParser :: Parser Statement
whileStatementParser = do
  symbol "STICK AROUND"
  condition <- termParser
  statements <- many statementParser
  symbol "CHILL"
  return (While condition statements)

-- call method statement, return statement, call read statement
statementParser :: Parser Statement
statementParser =
  assignmentParser <|> printStatementParser <|> intDeclarationStatementParser <|>
  ifStatementParser <|>
  whileStatementParser

mainMethodParser :: Parser AbstractMethod
mainMethodParser = do
  symbol "IT'S SHOWTIME"
  statements <- many statementParser
  symbol "YOU HAVE BEEN TERMINATED"
  return (Main statements)

argumentParser :: Parser MethodArg
argumentParser = do
  symbol "I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE"
  argument <- identifierParser
  return (MethodArg argument)

methodStatementsParser :: Parser [Statement]
methodStatementsParser = do
  symbol "GIVE THESE PEOPLE AIR"
  statements <- many statementParser
  return statements

methodParser :: Parser AbstractMethod
methodParser = do
  symbol "LISTEN TO ME VERY CAREFULLY"
  name <- identifierParser
  arguments <- many argumentParser
  statements <- try methodStatementsParser <|> (return [])
  symbol "HASTA LA VISTA, BABY"
  return (Method name arguments statements)

abstractMethodParser :: Parser AbstractMethod
abstractMethodParser = mainMethodParser <|> methodParser

programParser :: Parser Program
programParser = do
  methods <- some abstractMethodParser
  return methods
