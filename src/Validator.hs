module Validator where

import AST
import Control.Monad.Trans.State (State, get, runState, state)
import Data.List (foldl')
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
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
  | MethodAlreadyDeclaredError
  | MethodNotDeclaredError
  | MissingMainError
  | StoringResultFromVoidMethodError
  | ReturnsValueInVoidMethodError
  | MissingReturnValueInNonVoidMethodError
  | IllegalReturnStatementError
  | ExpectingReturnStatementError
  | DuplicateArgumentError
  deriving (Show, Eq)

data SymbolTable = SymbolTable
  { _parentTable :: Maybe SymbolTable
  , _variableSet :: Set String
  , _methodMap :: Map String ReturnType
  , _scope :: Scope
  } deriving (Show, Eq)

data Error = Error
  { _location :: R.Region
  , _errorType :: ErrorType
  } deriving (Show, Eq)

data Scope
  = GlobalScope
  | MainMethodScope
  | MethodScope String
                ReturnType
  | AnonymousScope
  deriving (Show, Eq)

emptyTable :: Scope -> SymbolTable
emptyTable scope =
  SymbolTable
    { _parentTable = Nothing
    , _variableSet = Set.empty
    , _methodMap = Map.empty
    , _scope = scope
    }

createChildSymbolTable :: Scope -> SymbolTable -> SymbolTable
createChildSymbolTable scope symbolTable =
  SymbolTable
    { _parentTable = Just symbolTable
    , _variableSet = Set.empty
    , _methodMap = Map.empty
    , _scope = scope
    }

-- will return the same table if there's no parent table
-- TODO: see if we can improve this
parentSymbolTable :: SymbolTable -> SymbolTable
parentSymbolTable symbolTable =
  case _parentTable symbolTable of
    Just table -> table
    Nothing -> symbolTable

mainMethodName :: String
mainMethodName = "main"

-- just some dummy position to report the 'missing main' error
mainMethodRegion :: R.Region
mainMethodRegion =
  R.Region
    { R._start = R.Position {R._line = 1, R._column = 1}
    , R._end = R.Position {R._line = 1, R._column = 1}
    }

-- 1. create global symbol table (all functions) -- done
-- 2. check that main method exists -- done
-- 3. validate all methods, each of which have their own symbol table -- done
validateAst :: Program -> Either [Error] Program
validateAst program =
  let (errors, globalSymbolTable) = createGlobalSymbolTable program
      doesMainMethodExist =
        Map.member mainMethodName (_methodMap globalSymbolTable)
   in case errors of
        [] ->
          if doesMainMethodExist
            then let moreErrors = validateMethods program globalSymbolTable
                  in case moreErrors of
                       [] -> Right program
                       _ -> Left $ reverse (moreErrors ++ errors) -- errors are preppended for efficiency 
                                                                  -- so we have to reverse them
            else Left $
                 [createErrorAt mainMethodRegion MissingMainError] ++ errors
        _ -> Left errors

type ValidationState = ([Error], SymbolTable)

createGlobalSymbolTable :: Program -> ValidationState
createGlobalSymbolTable methods =
  foldl' addMethod ([], (emptyTable GlobalScope)) methods
  where
    addMethod (errors, table) method =
      let (locatedMethodName, returnType) =
            case method of
              Main _ -> (R.At mainMethodRegion mainMethodName, TVoid)
              Method locatedName retType _ _ -> (locatedName, retType)
          methodName = R.unlocate locatedMethodName
          methodMap = _methodMap table
          alreadyExists = Map.member methodName methodMap
       in if alreadyExists
            then ( createError locatedMethodName MethodAlreadyDeclaredError :
                   errors
                 , table)
            else ( errors
                 , table
                     {_methodMap = Map.insert methodName returnType methodMap})

validateMethods :: Program -> SymbolTable -> [Error]
validateMethods methods symbolTable = foldl' validate [] methods
  where
    validate errors method = validateMethod method symbolTable ++ errors

validateMethod :: AbstractMethod -> SymbolTable -> [Error]
validateMethod method symbolTable =
  validateMethod' scope validationState symbolTable
  where
    scope =
      case method of
        Main _ -> MainMethodScope
        Method (R.At _ methodName) returnType _ _ ->
          MethodScope methodName returnType
    validationState =
      case method of
        Main locatedStatements -> validateStatements locatedStatements
        Method _ _ locatedArgs locatedStatements -> do
          validateArguments locatedArgs
          validateStatements locatedStatements
          validateReturnControlFlow locatedStatements

validateMethod' :: Scope -> State ValidationState () -> SymbolTable -> [Error]
validateMethod' scope validationState symbolTable =
  let methodSymbolTable = createChildSymbolTable scope symbolTable
      (_, finalState) = runState validationState initialState
        where
          initialState = ([], methodSymbolTable)
      errors = fst finalState
   in case errors of
        [] -> []
        _ -> errors

validateArguments :: [LocatedMethodArg] -> State ValidationState ()
validateArguments locatedArgs =
  state $ \(errors, symbolTable) ->
    let updatedTable = foldr addArgToTable symbolTable locatedArgs
          where
            addArgToTable locatedArg table = addArgumentToTable locatedArg table
     in case findDuplicates locatedArgs of
          [] -> return (errors, updatedTable)
          duplicates ->
            let errs = foldr createErr errors duplicates
                  where
                    createErr dup acc =
                      createError dup DuplicateArgumentError : acc
             in return (errs, updatedTable)

findDuplicates :: [LocatedMethodArg] -> [LocatedMethodArg]
findDuplicates args = findDuplicates' args [] Set.empty
  where
    findDuplicates' [] duplicates _ = duplicates
    findDuplicates' (arg':args') duplicates visitedSet =
      let unlocatedArg = R.unlocate arg'
       in if Set.member unlocatedArg visitedSet
            then findDuplicates' args' (arg' : duplicates) visitedSet
            else findDuplicates'
                   args'
                   duplicates
                   (Set.insert unlocatedArg visitedSet)

validateStatements :: [LocatedStatement] -> State ValidationState ()
validateStatements locatedStatements = mapM_ validateStatement locatedStatements

pushSymbolTable :: Scope -> State ValidationState ()
pushSymbolTable scope =
  state $ \(errors, symbolTable) ->
    return (errors, createChildSymbolTable scope symbolTable)

popSymbolTable :: State ValidationState ()
popSymbolTable =
  state $ \(errors, symbolTable) ->
    return (errors, parentSymbolTable symbolTable)

validateScopedStatements :: [LocatedStatement] -> State ValidationState ()
validateScopedStatements locatedStatements = do
  pushSymbolTable AnonymousScope
  validateStatements locatedStatements
  popSymbolTable

-- 1. Check all used variables are declared - done
-- 2. Check no duplicated variables - done
-- 3. Check all called methods exist - done
-- 4. Check variables do not clash with method arguments - done
-- 5. if-else blocks should push a new scope - done
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
      validateScopedStatements ifStatements
      validateScopedStatements elseStatements
      validateExpression locatedExpr
    While locatedExpr statements -> do
      validateScopedStatements statements
      validateExpression locatedExpr
    CallMethod maybeLocatedVarName locatedMethodName locatedArgs -> do
      validateExpressions locatedArgs
      validateMethodCall locatedMethodName maybeLocatedVarName
      validateMaybeVarUsage maybeLocatedVarName
    CallRead locatedVarName -> validateVarUsage locatedVarName
    Return maybeLocatedExpr -> do
      validateReturnContext locatedStatement
      validateMaybeExpression maybeLocatedExpr

-- Note: in contrast to official implementation, our if-else statements are scoped
-- and local variable declarations are allowed. If you do that with the official 
-- implementation it will throw a JNI error.
validateVarDeclaration :: LocatedVarName -> State ValidationState ()
validateVarDeclaration locatedVarName =
  state $ \(errors, symbolTable) ->
    if isVariableInTable locatedVarName symbolTable
      then return
             ( createError locatedVarName VarAlreadyDeclaredError : errors
             , symbolTable)
      else return (errors, addVariableToTable locatedVarName symbolTable)

validateMaybeVarUsage :: Maybe LocatedVarName -> State ValidationState ()
validateMaybeVarUsage (Just locatedVarName) = validateVarUsage locatedVarName
validateMaybeVarUsage Nothing = return ()

validateVarUsage :: LocatedVarName -> State ValidationState ()
validateVarUsage locatedVarName =
  state $ \(errors, symbolTable) ->
    if isVariableInScope locatedVarName symbolTable
      then return (errors, addVariableToTable locatedVarName symbolTable)
      else return
             ( createError locatedVarName VarNotDeclaredError : errors
             , symbolTable)

addVariableToTable :: LocatedVarName -> SymbolTable -> SymbolTable
addVariableToTable (R.At _ varName) table = addVariableToTable' varName table

addVariableToTable' :: String -> SymbolTable -> SymbolTable
addVariableToTable' varName table =
  let variableSet = _variableSet table
   in table {_variableSet = Set.insert varName variableSet}

addArgumentToTable :: LocatedMethodArg -> SymbolTable -> SymbolTable
addArgumentToTable (R.At _ (MethodArg argName)) table =
  addVariableToTable' argName table

isVariableInTable :: LocatedVarName -> SymbolTable -> Bool
isVariableInTable (R.At _ varName) symbolTable =
  Set.member varName (_variableSet symbolTable)

isVariableInScope :: LocatedVarName -> SymbolTable -> Bool
isVariableInScope locatedVarName symbolTable =
  if isVariableInTable locatedVarName symbolTable
    then True
    else case _parentTable symbolTable of
           Just parentTable -> isVariableInScope locatedVarName parentTable
           Nothing -> False

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

-- 1. Check called method exists -- done
-- 2. Check we're not calling a void method when storing result in a variable -- done
validateMethodCall ::
     LocatedMethodName -> Maybe LocatedVarName -> State ValidationState ()
validateMethodCall locatedMethodName maybeVarName =
  state $ \(errors, symbolTable) ->
    case findMethod locatedMethodName $ Just symbolTable of
      Left err -> return (err : errors, symbolTable)
      Right returnType ->
        case maybeVarName of
          Just _ ->
            case returnType of
              TVoid ->
                return
                  ( createError
                      locatedMethodName
                      StoringResultFromVoidMethodError :
                    errors
                  , symbolTable)
              TInt -> return (errors, symbolTable)
          Nothing -> return (errors, symbolTable)

findMethod :: LocatedMethodName -> Maybe SymbolTable -> Either Error ReturnType
findMethod locatedMethodName maybeSymbolTable =
  case maybeSymbolTable of
    Just symbolTable ->
      case lookupLocatedMethod locatedMethodName symbolTable of
        Just returnType -> Right returnType
        Nothing -> Left err
    Nothing -> Left err
  where
    err = createError locatedMethodName MethodNotDeclaredError

lookupLocatedMethod :: LocatedMethodName -> SymbolTable -> Maybe ReturnType
lookupLocatedMethod (R.At _ methodName) table = lookupMethod methodName table

lookupMethod :: String -> SymbolTable -> Maybe ReturnType
lookupMethod methodName table =
  let maybeReturnType = Map.lookup methodName (_methodMap table)
   in case maybeReturnType of
        Just returnType -> Just returnType
        Nothing ->
          case _parentTable table of
            Nothing -> Nothing
            Just parentTable -> lookupMethod methodName parentTable

validateReturnContext :: LocatedStatement -> State ValidationState ()
validateReturnContext locatedStatement = do
  (_, symbolTable) <- get
  case _scope symbolTable of
    MethodScope _ returnType ->
      validateReturnStatement locatedStatement returnType
    AnonymousScope ->
      let methodScope = findMethodScope symbolTable
       in case methodScope of
            MethodScope _ returnType ->
              validateReturnStatement locatedStatement returnType
            _ -> returnValidationError locatedStatement
    _ -> returnValidationError locatedStatement

returnValidationError :: LocatedStatement -> State ValidationState ()
returnValidationError locatedStatement =
  state $ \(errors, symbolTable) ->
    return
      ( createError locatedStatement IllegalReturnStatementError : errors
      , symbolTable)

validateReturnStatement ::
     LocatedStatement -> ReturnType -> State ValidationState ()
validateReturnStatement (R.At region (Return maybeLocatedReturnExpr)) returnType =
  state $ \(errors, symbolTable) ->
    case maybeLocatedReturnExpr of
      Just locatedReturnExpr ->
        case returnType of
          TInt -> return (errors, symbolTable)
          TVoid ->
            return
              ( createError locatedReturnExpr ReturnsValueInVoidMethodError :
                errors
              , symbolTable)
      Nothing ->
        case returnType of
          TInt ->
            return
              ( createErrorAt region MissingReturnValueInNonVoidMethodError :
                errors
              , symbolTable)
          TVoid -> return (errors, symbolTable)
validateReturnStatement locatedStatement _ =
  state $ \(errors, symbolTable) ->
    return
      ( createError locatedStatement ExpectingReturnStatementError : errors
      , symbolTable)

-- Check non-void functions must return in the last statement. This could be improved
-- by performing flow analysis but it's simpler to force the last statement to
-- be a valid return statement.
validateReturnControlFlow :: [LocatedStatement] -> State ValidationState ()
validateReturnControlFlow locatedStatements = do
  (_, symbolTable) <- get
  let methodScope = findMethodScope symbolTable
   in case methodScope of
        MethodScope _ TInt ->
          validateReturnStatement (last locatedStatements) TInt
        _ -> return ()

-- finds the method scope of the given symbol table. 
-- Only anonymous scopes (if-else/while blocks) have a parent method scope.
findMethodScope :: SymbolTable -> Scope
findMethodScope symbolTable =
  let scope = _scope symbolTable
   in case scope of
        AnonymousScope ->
          case _parentTable symbolTable of
            Nothing -> scope
            Just parentTable -> findMethodScope parentTable
        _ -> scope

createError :: R.Located a -> ErrorType -> Error
createError (R.At region _) errorType = createErrorAt region errorType

createErrorAt :: R.Region -> ErrorType -> Error
createErrorAt region errorType =
  Error {_location = region, _errorType = errorType}
