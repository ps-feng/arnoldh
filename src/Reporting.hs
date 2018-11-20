module Reporting where

import Data.Bifunctor (first)
import Region as R
import Text.Megaparsec.Error
  ( ParseError
  , ShowErrorComponent
  , ShowToken
  , parseErrorPretty
  )
import Validator as V

-- TODO: is there a way to merge the next 2 functions cleanly?
mapParserErrorToString ::
     (Ord t, ShowToken t, ShowErrorComponent e)
  => Either (ParseError t e) a
  -> Either String a
mapParserErrorToString result = first errorToString result
  where
    errorToString err = parseErrorPretty err

mapValidatorErrorToString ::
     FilePath -> String -> Either [V.Error] a -> Either String a
mapValidatorErrorToString filePath input result =
  let inputLines = lines input
      errToString err errStr = 
        validationErrorString err filePath inputLines ++ errStr
      errorsToString errs = foldr errToString "" errs
   in first errorsToString result

validationErrorString :: V.Error -> FilePath -> [String] -> String
validationErrorString err filePath inputLines =
  let location = V._location err
      startLine = R._line $ R._start location
      headline = filePath ++ ": line " ++ (show startLine)
      errorType = show (ValidationErrorType $ V._errorType err)
      errorLine = inputLines !! (startLine - 1)
      startColumn = (R._column $ R._start location) - 1
      endColumn = (R._column $ R._end location) - 1
      markerLen = endColumn - startColumn
      errorMarker = indent startColumn ++ markError markerLen
   in headline ++ eol 1 ++ 
      indent 4 ++ errorType ++ eol 2 ++ 
      indent 4 ++ errorLine ++ eol 1 ++
      indent 4 ++ errorMarker ++ eol 2

newtype ValidationErrorType =
  ValidationErrorType V.ErrorType

instance Show ValidationErrorType where
  show (ValidationErrorType VarAlreadyDeclaredError) =
    "Variable was already declared"
  show (ValidationErrorType VarNotDeclaredError) = "Variable was not declared"
  show (ValidationErrorType MethodAlreadyDeclaredError) =
    "Method was already declared"
  show (ValidationErrorType MethodNotDeclaredError) = "Method was not declared"
  show (ValidationErrorType MissingMainError) = "Missing main method"
  show (ValidationErrorType StoringResultFromVoidMethodError) =
    "Cannot store the result of a void method"
  show (ValidationErrorType ReturnsValueInVoidMethodError) =
    "Void method cannot return a value"
  show (ValidationErrorType MissingReturnValueInNonVoidMethodError) =
    "Non-void method should return a value"
  show (ValidationErrorType IllegalReturnStatementError) =
    "Can only return from a non-void method"
  show (ValidationErrorType ExpectingReturnStatementError) =
    "Expecting a return statement here"
  show (ValidationErrorType DuplicateArgumentError) = "Duplicated argument"

markError :: Int -> String
markError n = replicate n '^'

indent :: Int -> String
indent n = replicate n ' '

eol :: Int -> String
eol n = replicate n '\n'
