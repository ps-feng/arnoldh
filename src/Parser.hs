module Parser where

import AST
import Data.Functor (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

-- Useful links
-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
-- http://akashagrawal.me/beginners-guide-to-megaparsec/
type Parser = Parsec Void String

-- We define a space as a regular space or a tab character, as that's what
-- can separate an instruction and the identifier it applies to:
-- e.g. "LISTEN TO ME CAREFULLY   methodName"
spaceConsumer :: Parser ()
spaceConsumer = void $ many $ oneOf [' ', '\t']

-- Uses default built-in space skipper, which includes newlines.
eolConsumer :: Parser ()
eolConsumer = space1

-- this will trim all the whitespace after consuming the parsed lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

integerParser :: Parser Integer
integerParser = lexeme L.decimal

voidConsumer :: Parser ()
voidConsumer = return ()

signedIntegerParser :: Parser Integer
signedIntegerParser = L.signed voidConsumer integerParser

stringLiteralParser :: Parser String
stringLiteralParser = lexeme (char '"' >> manyTill L.charLiteral (char '"'))

booleanTermParser :: Parser Expr
booleanTermParser =
  (Int 0 <$ symbol "@I LIED") <|> (Int 1 <$ symbol "@NO PROBLEMO")

identifierParser :: Parser String
identifierParser = (lexeme . try) p
  where
    p = (:) <$> letterChar <*> many alphaNumChar

operandParser :: Parser Expr
operandParser =
  Int <$> signedIntegerParser <|> booleanTermParser <|> Var <$> identifierParser

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
  makeExprParser (operandParser <* eolConsumer) operatorTable

assignmentParser :: Parser Statement
assignmentParser = do
  symbol "GET TO THE CHOPPER"
  var <- identifierParser <* eolConsumer
  expr <- expressionParser
  symbol "ENOUGH TALK" >> eolConsumer
  return (Assignment var expr)

printStatementParser :: Parser Statement
printStatementParser = do
  symbol "TALK TO THE HAND"
  statement <- printExpressionStatementParser <|> printStringStatementParser
  return statement

printExpressionStatementParser :: Parser Statement
printExpressionStatementParser = do
  term <- try (operandParser <* eolConsumer)
  return (PrintExpr term)

printStringStatementParser :: Parser Statement
printStringStatementParser = do
  term <- try (stringLiteralParser <* eolConsumer)
  return (PrintStr term)

intVarDeclarationStatementParser :: Parser Statement
intVarDeclarationStatementParser = do
  symbol "HEY CHRISTMAS TREE"
  var <- identifierParser <* eolConsumer
  symbol "YOU SET US UP"
  number <- operandParser <* eolConsumer
  return (IntVar var number)

ifStatementParser :: Parser Statement
ifStatementParser = do
  symbol "BECAUSE I'M GOING TO SAY PLEASE"
  condition <- operandParser <* eolConsumer
  ifStatements <- many statementParser
  elseStatements <- try elseStatementParser <|> (return [])
  symbol "YOU HAVE NO RESPECT FOR LOGIC" >> eolConsumer
  return (If condition ifStatements elseStatements)

elseStatementParser :: Parser [Statement]
elseStatementParser = do
  symbol "BULLSHIT" >> eolConsumer
  elseStatements <- many statementParser
  return elseStatements

whileStatementParser :: Parser Statement
whileStatementParser = do
  symbol "STICK AROUND"
  condition <- operandParser <* eolConsumer
  statements <- many statementParser
  symbol "CHILL"
  return (While condition statements)

callMethodStatementParser :: Parser Statement
callMethodStatementParser = do
  var <-
    optional (symbol "GET YOUR ASS TO MARS" >> identifierParser <* eolConsumer)
  symbol "DO IT NOW"
  methodName <- identifierParser
  args <- many operandParser <* eolConsumer
  return (CallMethod var methodName args)

returnStatementParser :: Parser Statement
returnStatementParser = do
  symbol "I'LL BE BACK"
  retVal <-
    (eolConsumer *> return Nothing) <|>
    optional (try operandParser <* eolConsumer)
  return (Return retVal)

callReadMethodStatementParser :: Parser Statement
callReadMethodStatementParser = do
  symbol "GET YOUR ASS TO MARS"
  var <- identifierParser <* eolConsumer
  symbol "DO IT NOW" >> eolConsumer
  symbol
    "I WANT TO ASK YOU A BUNCH OF QUESTIONS AND I WANT TO HAVE THEM ANSWERED IMMEDIATELY"
  eolConsumer
  return (CallRead var)

statementParser :: Parser Statement
statementParser =
  assignmentParser <|> printStatementParser <|> intVarDeclarationStatementParser <|>
  ifStatementParser <|>
  whileStatementParser <|>
  callMethodStatementParser <|>
  returnStatementParser <|>
  callReadMethodStatementParser

mainMethodParser :: Parser AbstractMethod
mainMethodParser = do
  symbol "IT'S SHOWTIME" >> eolConsumer
  statements <- many statementParser
  symbol "YOU HAVE BEEN TERMINATED" >> eolConsumer
  return (Main statements)

argumentParser :: Parser MethodArg
argumentParser = do
  symbol "I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE"
  argument <- identifierParser <* eolConsumer
  return (MethodArg argument)

methodStatementsParser :: Parser [Statement]
methodStatementsParser = do
  symbol "GIVE THESE PEOPLE AIR" >> eolConsumer
  statements <- many statementParser
  return statements

methodParser :: Parser AbstractMethod
methodParser = do
  symbol "LISTEN TO ME VERY CAREFULLY"
  name <- identifierParser <* eolConsumer
  arguments <- many argumentParser
  statements <- try methodStatementsParser <|> (return [])
  symbol "HASTA LA VISTA, BABY" >> eolConsumer
  return (Method name arguments statements)

abstractMethodParser :: Parser AbstractMethod
abstractMethodParser = mainMethodParser <|> methodParser

-- TODO: parse error "WHAT THE FUCK DID I DO WRONG"
programParser :: Parser Program
programParser = do
  methods <- some abstractMethodParser
  return methods
