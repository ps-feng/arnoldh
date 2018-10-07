module Lib
    ( someFunc
    ) where

import Parser
import Text.Megaparsec

input = "I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg1"

someFunc :: IO ()
someFunc = do
    parseTest argumentParser input

