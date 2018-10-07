module Parser where

import AST
import Data.Functor (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

arnParseError = "WHAT THE FUCK DID I DO WRONG"

arnRead =
  "I WANT TO ASK YOU A BUNCH OF QUESTIONS AND I WANT TO HAVE THEM ANSWERED IMMEDIATELY"

arnCallMethod = "DO IT NOW"

arnAssignVariableFromMethodCall = "GET YOUR ASS TO MARS"

-- Useful links
-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
-- http://akashagrawal.me/beginners-guide-to-megaparsec/
type Parser = Parsec Void String

-- We define a space as a regular space or a tab character, as that's what
-- can separate an instruction and the identifier it operates on:
-- e.g. "LISTEN TO ME CAREFULLY   methodName"
spaceConsumer :: Parser ()
spaceConsumer = void $ many $ oneOf [' ', '\t']

-- Uses default built-in space skipper, which includes newlines.
eolConsumer :: Parser ()
eolConsumer = space1

trimEol :: Parser a -> Parser a
trimEol p = p <* (some space1)

-- this will trim all the whitespace after consuming the parsed lexeme
-- and consume at least one end of line
lexeme :: Parser a -> Parser a
lexeme = (trimEol . (L.lexeme spaceConsumer))

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

integerParser :: Parser Integer
integerParser = lexeme L.decimal

stringLiteralParser :: Parser String
stringLiteralParser = lexeme (char '"' >> manyTill L.charLiteral (char '"'))

booleanTermParser :: Parser Expr
booleanTermParser =
  (Int 0 <$ trimEol (symbol "@I LIED")) <|>
  (Int 1 <$ trimEol (symbol "@NO PROBLEMO"))

identifierParser :: Parser String
identifierParser = (lexeme . try) p
  where
    p = (:) <$> letterChar <*> many alphaNumChar

binaryOpTermParser :: Parser Expr
binaryOpTermParser =
  Int <$> integerParser <|> booleanTermParser <|> Var <$> identifierParser

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
  eolConsumer
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
  eolConsumer
  return (If condition ifStatements elseStatements)

elseStatementParser :: Parser [Statement]
elseStatementParser = do
  symbol "BULLSHIT"
  eolConsumer
  elseStatements <- many statementParser
  return elseStatements

whileStatementParser :: Parser Statement
whileStatementParser = do
  symbol "STICK AROUND"
  condition <- termParser
  statements <- many statementParser
  symbol "CHILL"
  return (While condition statements)

returnStatementParser :: Parser Statement
returnStatementParser = do
  symbol "I'LL BE BACK"
  retVal <- (eolConsumer *> return Nothing) <|> optional (try termParser)
  return (Return retVal)

statementParser :: Parser Statement
statementParser =
  assignmentParser <|> printStatementParser <|> intDeclarationStatementParser <|>
  ifStatementParser <|>
  whileStatementParser <|>
  returnStatementParser

mainMethodParser :: Parser AbstractMethod
mainMethodParser = do
  symbol "IT'S SHOWTIME"
  eolConsumer
  statements <- many statementParser
  symbol "YOU HAVE BEEN TERMINATED"
  eolConsumer
  return (Main statements)

argumentParser :: Parser MethodArg
argumentParser = do
  symbol "I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE"
  argument <- identifierParser
  return (MethodArg argument)

methodStatementsParser :: Parser [Statement]
methodStatementsParser = do
  symbol "GIVE THESE PEOPLE AIR"
  eolConsumer
  statements <- many statementParser
  return statements

methodParser :: Parser AbstractMethod
methodParser = do
  symbol "LISTEN TO ME VERY CAREFULLY"
  name <- identifierParser
  arguments <- many argumentParser
  statements <- try methodStatementsParser <|> (return [])
  symbol "HASTA LA VISTA, BABY"
  eolConsumer
  return (Method name arguments statements)

abstractMethodParser :: Parser AbstractMethod
abstractMethodParser = mainMethodParser <|> methodParser

programParser :: Parser Program
programParser = do
  methods <- some abstractMethodParser
  return methods
