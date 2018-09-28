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

spaceParser :: Parser ()
spaceParser = space

-- this will trim all the whitespace after consuming the parsed lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceParser

beginParser :: Parser String
beginParser = lexeme (string arnBeginMain)

endParser :: Parser String
endParser = lexeme (string arnEndMain)

setValueParser :: Parser String
setValueParser = lexeme (string arnSetValue)

integerParser :: Parser Integer
integerParser = lexeme L.decimal

stringParser :: Parser String
stringParser = char '"' >> manyTill L.charLiteral (char '"')

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
reservedWords = binaryOperators ++ [arnSetValue, arnPrint]

reservedWordParser :: Parser String
reservedWordParser = foldr1 (<|>) (map (lexeme . try . string) reservedWords)

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

--- Parser after this
ops :: [(String, Op)]
ops =
  [ (arnPlusOperator, Add)
  , (arnMinusOperator, Minus)
  , (arnDivisionOperator, Divide)
  , (arnMultiplicationOperator, Mult)
  , (arnModulo, Modulo)
  -- logical operators
  , (arnOr, Or)
  , (arnAnd, And)
  -- comparison
  , (arnEqualTo, EqualTo)
  , (arnGreaterThan, GreaterThan)
  ]

opMapper :: Parser String -> Parser Op
opMapper p =
  p >>=
  (\op ->
     case lookup op ops of
       Just x -> return x
       Nothing -> fail "unknown operator")

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

data PartialExpr =
  PartialExpr Op
              Expr

partialBinaryOperationParser :: Parser PartialExpr
partialBinaryOperationParser = do
  op <- opMapper binaryOperatorParser
  a <- binaryOpTermParser
  return (PartialExpr op a)

binaryExpressionParser :: Expr -> Parser Expr
binaryExpressionParser startValue = do
  partials <- many partialBinaryOperationParser
  expr <- binaryExpressionParser' startValue (return partials)
  return expr

binaryExpressionParser' :: Expr -> Parser [PartialExpr] -> Parser Expr
binaryExpressionParser' startExpr partialsParser = do
  partials <- partialsParser
  return $
    foldl (\acc (PartialExpr op a) -> BinaryOp op acc a) startExpr partials

expressionParser :: Parser Expr
expressionParser = do
  setValueParser
  startValue <- binaryOpTermParser
  expr <- binaryExpressionParser startValue
  return expr

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

statementParser :: Parser [Statement]
statementParser =
  some
    (assignmentParser <|> printStatementParser <|> intDeclarationStatementParser)

programParser :: Parser Program
programParser = do
  beginParser
  program <- statementParser
  endParser
  return program
