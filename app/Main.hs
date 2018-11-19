module Main where

import Lib
import Options.Applicative

data Args = Args
  { _filePath :: FilePath
  }

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (argsParser <**> helper) idm

run :: Args -> IO ()
run args = do
  let filePath = _filePath args
  fileStr <- readFile $ filePath
  compileProgram filePath fileStr

argsParser :: Parser Args
argsParser = Args <$> strArgument (metavar "TARGET" <> help "Path to file to compile")
