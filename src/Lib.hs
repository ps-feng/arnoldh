module Lib
    ( someFunc
    ) where

import Lexer
import Text.Megaparsec

someFunc :: IO ()
someFunc = do
    input <- getLine
    parseTest programParser input

