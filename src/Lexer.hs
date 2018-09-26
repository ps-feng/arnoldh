module Lexer where

import Data.Void
import Grammar
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
  ]

arithOperatorParser :: Parser String
arithOperatorParser = foldr1 (<|>) (map (lexeme . try . string) arithOperators)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
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

arithInitialExpressionParser :: Parser ArithExpr
arithInitialExpressionParser = do
  setValueParser
  a <- intToIntConst integerParser
  op <- arithOpMapper arithOperatorParser
  b <- intToIntConst integerParser
  return (ArithBinary op a b)

arithPartialExpressionParser :: ArithExpr -> Parser ArithExpr
arithPartialExpressionParser prevExpr = do
  op <- arithOpMapper arithOperatorParser
  a <- intToIntConst integerParser
  return (ArithBinary op a prevExpr)

-- statementParser :: Parser Statement
-- statementParser = do
--   startAssignParser
--   id <- identifier
--   expr <- arithExpressionParser
--   endAssignParser
--   return (Assignment "dummy" expr)

programParser :: Parser String
programParser = do
  beginParser
  -- statement parser here
  expr <- arithInitialExpressionParser
  some (arithPartialExpressionParser expr)
  endParser
