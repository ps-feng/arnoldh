module Lib
  ( compileProgram
  ) where

import AST (Program)
import Generation.JavaScript as Js
import Parser (programParser)
import Reporting
  ( mapParserErrorToString
  , mapValidatorErrorToString
  , prettyPrintErrors
  )
import Text.Megaparsec
import Validator

compileProgram :: FilePath -> String -> IO ()
compileProgram filePath input =
  let validation = do
        parsedProgram <-
          mapParserErrorToString $ parse programParser filePath input
        validatedProgram <-
          mapValidatorErrorToString filePath input $ validateAst parsedProgram
        Right validatedProgram
   in case validation of
        Left err -> prettyPrintErrors err
        Right program -> generateCode (filePath ++ ".js") program

generateCode :: FilePath -> Program -> IO ()
generateCode filePath program =
  let jsCode = Js.generate program
   in writeFile filePath jsCode
