module Lib
  ( compileProgram
  ) where

import Parser (programParser)
import Reporting (mapParserErrorToString, mapValidatorErrorToString)
import Text.Megaparsec
import Validator

compileProgram :: FilePath -> String -> IO ()
compileProgram filePath input =
  let validation = do
        parsedProgram <-
          mapParserErrorToString $ parse programParser filePath input
        _ <-
          mapValidatorErrorToString filePath input $ validateAst parsedProgram
        return $ Right parsedProgram
   in case validation of
        Left err -> putStr err
        Right _ -> putStrLn "To guay"
