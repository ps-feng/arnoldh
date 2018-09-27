module Lib
    ( someFunc
    ) where

import Lexer
import Text.Megaparsec

input = "IT'S SHOWTIME \
        \GET TO THE CHOPPER myvar \
        \HERE IS MY INVITATION 4 \
        \GET UP 2 \
        \GET UP 4 \
        \GET DOWN 5 \
        \ENOUGH TALK \
        \YOU HAVE BEEN TERMINATED"

someFunc :: IO ()
someFunc = do
    parseTest programParser input

