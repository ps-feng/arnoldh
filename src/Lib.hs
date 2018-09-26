module Lib
    ( someFunc
    ) where

import Lexer
import Text.Megaparsec

input = "IT'S SHOWTIME \
        \HERE IS MY INVITATION 4 \
        \GET UP 2 \
        \GET UP 4 \
        \YOU HAVE BEEN TERMINATED"

someFunc :: IO ()
someFunc = do
    parseTest programParser input

