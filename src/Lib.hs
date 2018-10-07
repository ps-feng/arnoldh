module Lib
    ( someFunc
    ) where

import Parser
import Text.Megaparsec

input = "LISTEN TO ME VERY CAREFULLY aMethod\n\
\HASTA LA VISTA, BABY\n\
\\
\IT'S SHOWTIME\n\
\YOU HAVE BEEN TERMINATED\n\
\\
\LISTEN TO ME VERY CAREFULLY aMethod2\n\
\I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg1\n\
\I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg2\n\
\HASTA LA VISTA, BABY\n"

someFunc :: IO ()
someFunc = do
    parseTest programParser input

