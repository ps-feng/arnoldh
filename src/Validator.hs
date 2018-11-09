module Validator where

import AST
import Control.Monad.Trans.State
import Data.Set (Set)

import qualified Data.Set as Set
import qualified Region as R

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
data ErrorType
  = VarAlreadyDeclaredError
  | VarNotDeclaredError
  | MethodNotDeclaredError
  deriving (Eq)

instance Show ErrorType where
  show VarAlreadyDeclaredError = "Variable already declared"
  show VarNotDeclaredError = "Variable was not declared"
  show MethodNotDeclaredError = "Called method was not declared"

data SymbolTable = SymbolTable
  { _parentTable :: Maybe SymbolTable
  , _variableSet :: Set String
  , _methodSet :: Set String
  , _currentMethod :: String
  } deriving (Eq, Show)

data Error = Error
  { _location :: R.Region
  , _errorMsg :: ErrorType
  } deriving (Eq, Show)

emptyTable :: String -> SymbolTable
emptyTable methodName =
  SymbolTable
    { _parentTable = Nothing
    , _variableSet = Set.empty
    , _methodSet = Set.empty
    , _currentMethod = methodName
    }

validateAst :: Program -> Either String Program
validateAst program = Right program

-- only method names are stored in the global table
createGlobalSymbolTable :: Program -> SymbolTable
createGlobalSymbolTable methods = foldr addMethod initialTable methods
  where
    initialTable = emptyTable ""
    addMethod =
      \locatedMethod table ->
        let name =
              case locatedMethod of
                Main _ -> ""
                Method locatedName _ _ -> R.unlocate locatedName
            methodSet = _methodSet table
         in table {_methodSet = Set.insert name methodSet}

type ValidationState = ([Error], SymbolTable)

-- check in method declaration that argument names are not repeated
-- validate statements in method
validateMethod :: AbstractMethod -> SymbolTable -> Either [Error] SymbolTable
validateMethod (Main locatedStatements) symbolTable =
  let (_, finalState) =
        runState (validateStatements locatedStatements) initialState
        where
          initialState = ([], symbolTable)
      errors = fst finalState
   in case errors of
        [] -> Right (snd finalState)
        _ -> Left $ reverse errors -- errors are preppended for efficiency 
                                   -- so we have to reverse them
                                   -- TODO: might have to move this to an upper level
validateMethod (Method _ _ _) _ = Left [] -- TODO

validateStatements :: [LocatedStatement] -> State ValidationState ()
validateStatements locatedStatements = mapM_ validateStatement locatedStatements

-- check all used variables are declared - done
-- check no duplicated variables - done
-- check all called methods exist - done
-- check variables do not clash with method arguments - done
-- There appears to be only 2 scopes in the language (if/else/while 
-- don't create new scopes):
-- - Main function scope
-- - Function scopes
validateStatement :: LocatedStatement -> State ValidationState ()
validateStatement locatedStatement =
  case R.unlocate locatedStatement of
    Assignment locatedVarName locatedExpr -> do
      validateExpression locatedExpr
      validateVarUsage locatedVarName
    PrintExpr locatedExpr -> validateExpression locatedExpr
    PrintStr _ -> return ()
    IntVar locatedVarName locatedExpr -> do
      validateExpression locatedExpr
      validateVarDeclaration locatedVarName
    If locatedExpr ifStatements elseStatements -> do
      validateStatements ifStatements
      validateStatements elseStatements
      validateExpression locatedExpr
    While locatedExpr statements -> do
      validateStatements statements
      validateExpression locatedExpr
    CallMethod maybeLocatedVarName locatedMethodName locatedArgs -> do
      validateExpressions locatedArgs
      validateMethodCall locatedMethodName
      validateMaybeVarUsage maybeLocatedVarName
    CallRead locatedVarName -> validateVarUsage locatedVarName
    Return maybeLocatedExpr -> validateMaybeExpression maybeLocatedExpr

-- TODO: ideally we should not allow variable declaration within if-else statements.
-- Current official implementation throws a JNI exception.
validateVarDeclaration :: LocatedVarName -> State ValidationState ()
validateVarDeclaration locatedVarName =
  state $ \(errors, symbolTable) ->
    if containsVariable locatedVarName symbolTable
      then return
             ( Error
                 { _location = R.getLocation locatedVarName
                 , _errorMsg = VarAlreadyDeclaredError
                 } :
               errors
             , symbolTable)
      else return (errors, addVariableToTable locatedVarName symbolTable)

validateMaybeVarUsage :: Maybe LocatedVarName -> State ValidationState ()
validateMaybeVarUsage (Just locatedVarName) = validateVarUsage locatedVarName
validateMaybeVarUsage Nothing = return ()

validateVarUsage :: LocatedVarName -> State ValidationState ()
validateVarUsage locatedVarName =
  state $ \(errors, symbolTable) ->
    if containsVariable locatedVarName symbolTable
      then return (errors, addVariableToTable locatedVarName symbolTable)
      else return
             ( Error
                 { _location = R.getLocation locatedVarName
                 , _errorMsg = VarNotDeclaredError
                 } :
               errors
             , symbolTable)

addVariableToTable :: LocatedVarName -> SymbolTable -> SymbolTable
addVariableToTable (R.At _ varName) table =
  let variableSet = _variableSet table
   in table {_variableSet = Set.insert varName variableSet}

containsVariable :: LocatedVarName -> SymbolTable -> Bool
containsVariable (R.At _ varName) table =
  Set.member varName (_variableSet table)

validateExpressions :: [LocatedExpr] -> State ValidationState ()
validateExpressions locatedExprs = mapM_ validateExpression locatedExprs

validateExpression :: LocatedExpr -> State ValidationState ()
validateExpression (R.At region expr) =
  case expr of
    Int _ -> return ()
    Var name -> validateVarUsage (R.At region name)
    BinaryOp _ expr1 expr2 -> do
      validateExpression expr1
      validateExpression expr2

validateMaybeExpression :: Maybe LocatedExpr -> State ValidationState ()
validateMaybeExpression (Just expr) = validateExpression expr
validateMaybeExpression Nothing = return ()

validateMethodCall :: LocatedMethodName -> State ValidationState ()
validateMethodCall locatedMethodName =
  state $ \(errors, symbolTable) ->
    let maybeError = findMethod locatedMethodName $ Just symbolTable
     in case maybeError of
          Just err -> return (err : errors, symbolTable)
          Nothing -> return (errors, symbolTable)

findMethod :: LocatedMethodName -> Maybe SymbolTable -> Maybe Error
findMethod locatedMethodName maybeSymbolTable =
  case maybeSymbolTable of
    Just symbolTable ->
      if containsMethod locatedMethodName symbolTable
        then Nothing
        else findMethod locatedMethodName $ _parentTable symbolTable
    Nothing ->
      Just
        Error
          { _location = R.getLocation locatedMethodName
          , _errorMsg = MethodNotDeclaredError
          }

containsMethod :: LocatedMethodName -> SymbolTable -> Bool
containsMethod (R.At _ methodName) table =
  Set.member methodName (_methodSet table)
