module Reporting
  ( mapParserErrorToString
  , mapValidatorErrorToString
  , prettyPrintErrors
  ) where

import Data.Bifunctor (first)
import Text.Megaparsec.Error
  ( ParseError
  , ShowErrorComponent
  , ShowToken
  , parseErrorPretty
  )

import qualified Formatting as F
import qualified Region as R
import qualified Validator as V

prettyPrintErrors :: String -> IO ()
prettyPrintErrors errs =
  putStr $
  F.eol1 ++
  commonHeader ++ F.eol2 ++
  errs

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
      errorMarker = F.indent startColumn ++ F.caret markerLen
   in headline ++ F.eol1 ++
      F.indent4 ++ errorType ++ F.eol2 ++
      F.indent4 ++ errorLine ++ F.eol1 ++
      F.indent4 ++ errorMarker ++ F.eol2

newtype ValidationErrorType =
  ValidationErrorType V.ErrorType

instance Show ValidationErrorType where
  show (ValidationErrorType V.VarAlreadyDeclaredError) =
    "Variable was already declared"
  show (ValidationErrorType V.VarNotDeclaredError) = "Variable was not declared"
  show (ValidationErrorType V.MethodAlreadyDeclaredError) =
    "Method was already declared"
  show (ValidationErrorType V.MethodNotDeclaredError) =
    "Method was not declared"
  show (ValidationErrorType V.MissingMainError) = "Missing main method"
  show (ValidationErrorType V.StoringResultFromVoidMethodError) =
    "Cannot store the result of a void method"
  show (ValidationErrorType V.ReturnsValueInVoidMethodError) =
    "Void method cannot return a value"
  show (ValidationErrorType V.MissingReturnValueInNonVoidMethodError) =
    "Non-void method should return a value"
  show (ValidationErrorType V.IllegalReturnStatementError) =
    "Can only return from a non-void method"
  show (ValidationErrorType V.ExpectingReturnStatementError) =
    "Expecting a return statement here"
  show (ValidationErrorType V.DuplicateArgumentError) = "Duplicated argument"

commonHeader :: String
commonHeader =
  "****************************\n\
  \WHAT THE FUCK DID I DO WRONG\n\
  \****************************"
