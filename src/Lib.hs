module Lib
    ( someFunc
    ) where

import Lexer
import Text.Megaparsec

input = "BECAUSE I'M GOING TO SAY PLEASE a \
        \TALK TO THE HAND \"a is true\" \
        \TALK TO THE HAND \"b might be true\" \
        \BULLSHIT \
        \YOU HAVE NO RESPECT FOR LOGIC"

someFunc :: IO ()
someFunc = do
    parseTest ifStatementParser input

