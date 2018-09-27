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

arnPrint = "TALK TO THE HAND"

arnRead =
  "I WANT TO ASK YOU A BUNCH OF QUESTIONS AND I WANT TO HAVE THEM ANSWERED IMMEDIATELY"

arnAssignVariable = "GET TO THE CHOPPER"

arnSetValue = "HERE IS MY INVITATION"

arnEndAssignVariable = "ENOUGH TALK"

arnFalse = "I LIED"

arnTrue = "NO PROBLEMO"

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

arnModulo = "I LET HIM GO"

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

startAssignParser :: Parser String
startAssignParser = lexeme (string arnAssignVariable)

endAssignParser :: Parser String
endAssignParser = lexeme (string arnEndAssignVariable)

reservedWords :: [String]
reservedWords =
  [ arnPlusOperator
  , arnMinusOperator
  , arnDivisionOperator
  , arnMultiplicationOperator
  , arnSetValue
  ]

reservedWordParser :: Parser String
reservedWordParser = foldr1 (<|>) (map (lexeme . try . string) reservedWords)

arithOperators =
  [ arnPlusOperator
  , arnMinusOperator
  , arnDivisionOperator
  , arnMultiplicationOperator
  , arnModulo
  ]

arithOperatorParser :: Parser String
arithOperatorParser = foldr1 (<|>) (map (lexeme . try . string) arithOperators)

identifierParser :: Parser String
identifierParser = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

--- Parser after this
arithOps :: [(String, ArithBinaryOp)]
arithOps =
  [ (arnPlusOperator, Add)
  , (arnMinusOperator, Minus)
  , (arnDivisionOperator, Divide)
  , (arnMultiplicationOperator, Mult)
  ]

arithOpMapper :: Parser String -> Parser ArithBinaryOp
arithOpMapper p =
  p >>=
  (\op ->
     case lookup op arithOps of
       Just x -> return x
       Nothing -> fail "unknown operator")

intToIntConst :: Parser Integer -> Parser ArithExpr
intToIntConst x = x >>= (\x' -> return (IntConst x'))

identifierToVarParser :: Parser String -> Parser ArithExpr
identifierToVarParser x = x >>= (\x' -> return (Var x'))

arithTermParser :: Parser ArithExpr
arithTermParser = intToIntConst integerParser <|> identifierToVarParser identifierParser

data PartialArithExpr =
  PartialArith ArithBinaryOp
               ArithExpr

arithPartialExpressionParser :: Parser PartialArithExpr
arithPartialExpressionParser = do
  op <- arithOpMapper arithOperatorParser
  a <- arithTermParser
  return (PartialArith op a)

arithExpressionParser :: ArithExpr -> Parser ArithExpr
arithExpressionParser startValue = do
  partials <- many arithPartialExpressionParser
  expr <- arithExpressionParser' startValue (return partials)
  return expr

arithExpressionParser' ::
     ArithExpr -> Parser [PartialArithExpr] -> Parser ArithExpr
arithExpressionParser' startExpr partialsParser = do
  partials <- partialsParser
  return $
    foldl (\acc (PartialArith op a) -> ArithBinary op acc a) startExpr partials

assignmentParser :: Parser Statement
assignmentParser = do
  startAssignParser
  id <- identifierParser
  setValueParser
  startValue <- arithTermParser
  expr <- arithExpressionParser startValue
  endAssignParser
  return (Assignment id expr)

statementParser :: Parser [Statement]
statementParser = some assignmentParser

programParser :: Parser Program
programParser = do
  beginParser
  program <- statementParser
  endParser
  return program
