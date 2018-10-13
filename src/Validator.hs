module Validator where

import AST
import Data.Set (Set)

-- After parsing, semantic validation of the AST needs to be performed.
-- This involves checking that all used variables are declared,
-- called methods exist, no duplicated variables, etc.
--
-- Useful links
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
-- https://tomassetti.me/building-compiler-language-validation/
-- https://dsprenkels.com/compiler-construction.html
--
--
data SymbolTable = SymbolTable
  { variablesTable :: String
  , methodsTable :: String
  , currentMethod :: String
  }

emptyTable methodName = SymbolTable { variablesTable = "", methodsTable = "", currentMethod = methodName}

validateAst :: Program -> Either String Program
validateAst program = Right program
--checkAllUsedVariablesAreDeclared
-- check all called methods exist
-- check no duplicated variables

-- createSymbolTable :: AbstractMethod -> SymbolTable
-- createSymbolTable (Main statements) =
--   let
--     symbolTable = emptyTable "Main"
--     variableSet = Set.empty
--     reversedStatements = reverse statements
--   in
--     foldr 
-- createSymbolTable (Method methodName args statements) = emptyTable methodName
