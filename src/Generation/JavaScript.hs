module Generation.JavaScript
  ( generate
  ) where

import AST

import qualified Data.List as List
import qualified Formatting as F
import qualified Region as R

type Indentation = String

-- Heavily inspired by Elm's compiler (JavaScript/Builder.hs)
--
-- Other useful resources
-- - https://stackoverflow.com/a/7610887/328616
-- - https://www.w3schools.com/js/js_hoisting.asp
-- TODO: change to Text or ByteString
generate :: Program -> String
generate methods = foldr generateAbstractMethod "" methods

generateAbstractMethod :: AbstractMethod -> String -> String
generateAbstractMethod method generatedCode =
  case method of
    Main locatedStatements -> generateMain locatedStatements ++ F.eol2 ++ generatedCode
    Method locatedMethodName _ locatedArgs locatedStatements ->
      (generateMethod locatedMethodName locatedArgs locatedStatements) ++ F.eol2 ++
      generatedCode

generateMain :: [LocatedStatement] -> String
generateMain locatedStatements =
  "function main() {\n" ++
  generateStatements (F.indent2) (unlocateL locatedStatements) ++
  "}\n" ++
  "main();"

generateMethod ::
     LocatedMethodName -> [LocatedMethodArg] -> [LocatedStatement] -> String
generateMethod (R.At _ methodName) locatedArgs locatedStatements =
  let args = (map argName . unlocateL) locatedArgs
      statements = unlocateL locatedStatements
   in "function " ++ methodName ++ "(" ++ commaSep args ++ ") {\n" ++
      generateStatements (F.indent2) statements ++
      "}"

generateStatements :: Indentation -> [Statement] -> String
generateStatements indentation statements =
  concat $ map (generateStatement indentation) statements

generateStatement :: Indentation -> Statement -> String
generateStatement indentation statement =
  case statement of
    Assignment (R.At _ varName) (R.At _ expr) ->
      generateAssignment indentation varName expr
    PrintExpr (R.At _ expr) ->
      generateLogToConsole indentation $ generateExpression expr
    PrintStr str -> generateLogToConsole indentation (quotesFor str)
    IntVar (R.At _ varName) (R.At _ expr) ->
      generateIntVarDeclaration indentation varName expr
    If (R.At _ conditionExpr) ifStatements elseStatements ->
      generateIfElse indentation conditionExpr ifStatements elseStatements
    While (R.At _ conditionExpr) statements ->
      generateWhile indentation conditionExpr statements
    CallMethod maybeVar (R.At _ methodName) locatedArgs ->
      generateMethodCall indentation maybeVar methodName locatedArgs
    CallRead _ -> "" -- TODO
    Return maybeLocatedExpr -> generateReturn indentation maybeLocatedExpr

generateExpression :: Expr -> String
generateExpression expr =
  case expr of
    Int num -> show num
    Var varName -> varName
    BinaryOp Divide (R.At _ expr1) (R.At _ expr2) ->
      -- division in JS will return floats and we want integer division
      "Math.floor(" ++ 
      parensFor (generateExpression expr1 ++ opToStr Divide ++ generateExpression expr2) ++ 
      ")"
    BinaryOp op (R.At _ expr1) (R.At _ expr2) ->
      parensFor $ generateExpression expr1 ++ opToStr op ++ generateExpression expr2

opToStr :: Op -> String
opToStr op =
  case op of
    Add -> " + "
    Minus -> " - "
    Mult -> " * "
    Divide -> " / "
    Modulo -> " % "
    Or -> " || "
    And -> " && "
    EqualTo -> " == "
    GreaterThan -> " > "

generateAssignment :: Indentation -> String -> Expr -> String
generateAssignment indentation varName expr =
  generateAssignment' indentation varName $ generateExpression expr

generateAssignment' :: Indentation -> String -> String -> String
generateAssignment' indentation varName exprStr = indentation ++ varName ++ " = " ++ exprStr ++ eol

generateLogToConsole :: Indentation -> String -> String
generateLogToConsole indentation str = indentation ++ "console.log(" ++ str ++ ")" ++ eol

generateIntVarDeclaration :: Indentation -> String -> Expr -> String
generateIntVarDeclaration indentation varName expr =
  indentation ++ "var " ++ varName ++ " = " ++ generateExpression expr ++ eol

generateIfElse :: Indentation -> Expr -> [LocatedStatement] -> [LocatedStatement] -> String
generateIfElse indentation conditionExpr locatedIfStatements locatedElseStatements =
  F.eol1 ++
  indentation ++ "if (" ++ generateExpression conditionExpr ++ ") {\n" ++
  generateStatements (deeperIndent indentation) (unlocateL locatedIfStatements) ++
  indentation ++ "} else {\n" ++
  generateStatements (deeperIndent indentation) (unlocateL locatedElseStatements) ++ 
  indentation ++ "}\n"

generateReturn :: Indentation -> Maybe LocatedExpr -> String
generateReturn indentation maybeLocatedExpr =
  indentation ++
  case maybeLocatedExpr of
    Nothing -> "return" ++ eol
    Just (R.At _ expr) -> "return " ++ generateExpression expr ++ eol

generateWhile :: Indentation -> Expr -> [LocatedStatement] -> String
generateWhile indentation conditionExpr statements =
  F.eol1 ++
  indentation ++ "while (" ++ generateExpression conditionExpr ++ ") {\n" ++
  generateStatements (deeperIndent indentation) (unlocateL statements) ++
  indentation ++ "}\n"

generateMethodCall :: Indentation -> Maybe LocatedVarName -> String -> [LocatedExpr] -> String
generateMethodCall indentation maybeVarName methodName locatedArgs =
  let args = (map generateExpression . unlocateL) locatedArgs
      methodCall = methodName ++ "(" ++ commaSep args ++ ")"
   in case maybeVarName of
        Nothing -> indentation ++ methodCall ++ eol
        Just varName ->
          generateAssignment' indentation (R.unlocate varName) methodCall

unlocateL :: [R.Located a] -> [a]
unlocateL = map (R.unlocate)

commaSep :: [String] -> String
commaSep = List.intercalate ", "

parensFor :: String -> String
parensFor str = "(" ++ str ++ ")"

quotesFor :: String -> String
quotesFor str = "\"" ++ str ++ "\""

argName :: MethodArg -> String
argName (MethodArg name) = name

deeperIndent :: String -> String
deeperIndent ind = ind ++ F.indent2

eol :: String
eol = ";\n"
