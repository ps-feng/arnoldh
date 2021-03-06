{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser
  ( expressionParser
  , assignmentParser
  , printStatementParser
  , intVarDeclarationStatementParser
  , ifStatementParser
  , whileStatementParser
  , callMethodStatementParser
  , returnStatementParser
  , callReadMethodStatementParser
  , statementParser
  , mainMethodParser
  , argumentParser
  , methodStatementsParser
  , methodParser
  , abstractMethodParser
  , programParser
  ) where

import AST
import Control.Applicative (liftA2)
import Data.Functor (void)
import Data.Void
import qualified Region as R
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

voidConsumer :: Parser ()
voidConsumer = return ()

-- Uses default built-in space skipper, which includes newlines.
eolConsumer :: Parser ()
eolConsumer = space1

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

signedIntegerParser :: Parser Integer
signedIntegerParser = L.signed voidConsumer L.decimal

booleanTermParser :: Parser Integer
booleanTermParser = (0 <$ symbol "@I LIED") <|> (1 <$ symbol "@NO PROBLEMO")

identifierParser :: Parser String
identifierParser = liftA2 (:) letterChar (many alphaNumChar)

stringLiteralParser :: Parser String
stringLiteralParser = char '"' >> manyTill L.charLiteral (char '"')

-- TODO: investigate other approaches as the parsers defined in this file
-- will generally consume the trailing spaces so the spaces end up counting towards
-- the final position
locatedParser :: Parser a -> Parser (R.Located a)
locatedParser parser = do
  start <- getParserPosition
  val <- parser
  end <- getParserPosition
  return (R.at start end val)
  where
    getParserPosition :: Parser R.Position
    getParserPosition = do
      pos <- getPosition
      return $
        R.Position
          { R._line = unPos . sourceLine $ pos
          , R._column = unPos . sourceColumn $ pos
          }

operandParser :: Parser LocatedExpr
operandParser =
  (fmap Int) <$> locatedParser signedIntegerParser <|>
  (fmap Int) <$> locatedParser booleanTermParser <|>
  (fmap Var) <$> locatedParser identifierParser

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

operatorTable :: [[Operator Parser LocatedExpr]]
operatorTable = [map (\(op, sym) -> InfixL (opParser op sym)) ops]
  where
    opParser ::
         Op -> String -> Parser (LocatedExpr -> LocatedExpr -> LocatedExpr)
    opParser op sym = do
      locatedOp <- locatedParser $ symbol sym
      return $ (fmap (.) (flip R.locate) locatedOp) . (BinaryOp op)

expressionParser :: Parser LocatedExpr
expressionParser = do
  symbol "HERE IS MY INVITATION"
  makeExprParser (operandParser <* eolConsumer) operatorTable

assignmentParser :: Parser Statement
assignmentParser = do
  symbol "GET TO THE CHOPPER"
  var <- locatedParser identifierParser <* eolConsumer
  expr <- expressionParser
  symbol "ENOUGH TALK" >> eolConsumer
  return (Assignment var expr)

printExpressionStatementParser :: Parser Statement
printExpressionStatementParser = do
  term <- try (operandParser <* eolConsumer)
  return (PrintExpr term)

printStringStatementParser :: Parser Statement
printStringStatementParser = do
  term <- try (stringLiteralParser <* eolConsumer)
  return (PrintStr term)

printStatementParser :: Parser Statement
printStatementParser = do
  symbol "TALK TO THE HAND"
  statement <- printExpressionStatementParser <|> printStringStatementParser
  return statement

intVarDeclarationStatementParser :: Parser Statement
intVarDeclarationStatementParser = do
  symbol "HEY CHRISTMAS TREE"
  var <- locatedParser identifierParser <* eolConsumer
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

elseStatementParser :: Parser [LocatedStatement]
elseStatementParser = do
  symbol "BULLSHIT" >> eolConsumer
  elseStatements <- many statementParser
  return elseStatements

whileStatementParser :: Parser Statement
whileStatementParser = do
  symbol "STICK AROUND"
  condition <- operandParser <* eolConsumer
  statements <- many statementParser
  symbol "CHILL" >> eolConsumer
  return (While condition statements)

callMethodStatementParser :: Parser Statement
callMethodStatementParser = do
  var <-
    optional
      (symbol "GET YOUR ASS TO MARS" >>
       locatedParser identifierParser <* eolConsumer)
  symbol "DO IT NOW"
  methodName <- locatedParser identifierParser <* spaceConsumer
  args <- many (operandParser <* spaceConsumer) <* eolConsumer
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
  var <- locatedParser identifierParser <* eolConsumer
  symbol "DO IT NOW" >> eolConsumer
  symbol
    "I WANT TO ASK YOU A BUNCH OF QUESTIONS AND I WANT TO HAVE THEM ANSWERED IMMEDIATELY"
  eolConsumer
  return (CallRead var)

statementParser :: Parser LocatedStatement
statementParser =
  foldr1 (<|>) $
  map
    -- TODO: check if this is correct because the resulting location will wrap the
    -- whole instruction until the beginning of the next one (including all newlines)
    locatedParser
    [ assignmentParser
    , printStatementParser
    , intVarDeclarationStatementParser
    , ifStatementParser
    , whileStatementParser
    , callMethodStatementParser
    , returnStatementParser
    , callReadMethodStatementParser
    ]

mainMethodParser :: Parser AbstractMethod
mainMethodParser = do
  symbol "IT'S SHOWTIME" >> eolConsumer
  statements <- many statementParser
  symbol "YOU HAVE BEEN TERMINATED" >> eolConsumer
  return (Main statements)

argumentParser :: Parser LocatedMethodArg
argumentParser = do
  symbol "I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE"
  argument <- locatedParser identifierParser
  return (MethodArg <$> argument)

methodStatementsParser :: Parser [LocatedStatement]
methodStatementsParser = do
  statements <- many statementParser
  return statements

nonVoidMethodTypeParser :: Parser ReturnType
nonVoidMethodTypeParser = do
  symbol "GIVE THESE PEOPLE AIR" >> eolConsumer
  return TInt

methodParser :: Parser AbstractMethod
methodParser = do
  symbol "LISTEN TO ME VERY CAREFULLY"
  name <- locatedParser identifierParser <* eolConsumer
  arguments <- many $ argumentParser <* eolConsumer
  returnType <- try nonVoidMethodTypeParser <|> return TVoid
  statements <- try methodStatementsParser <|> return []
  symbol "HASTA LA VISTA, BABY" >> space
  return (Method name returnType arguments statements)

abstractMethodParser :: Parser AbstractMethod
abstractMethodParser = mainMethodParser <|> methodParser

programParser :: Parser Program
programParser = do
  methods <- some abstractMethodParser
  return methods
