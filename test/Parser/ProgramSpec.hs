module Parser.ProgramSpec where

import AST
import Parser
import qualified Region as R
import Test.Hspec
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
  describe "program parser" $ do
    it "should parse main and all other methods" $ do
      parseMaybe
        programParser
        "LISTEN TO ME VERY CAREFULLY aMethod\n\
        \HASTA LA VISTA, BABY\n\
        \    \
        \IT'S SHOWTIME\n\
        \YOU HAVE BEEN TERMINATED\n\
        \\
        \LISTEN TO ME VERY CAREFULLY aMethod2\n\
        \I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg1\n\
        \I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTORCYCLE arg2\n\
        \HASTA LA VISTA, BABY\n" `shouldBe`
        Just
          [ Method
              (R.At
                 (R.Region
                    { R._start = R.Position {R._line = 1, R._column = 29}
                    , R._end = R.Position {R._line = 1, R._column = 36}
                    })
                 "aMethod")
              TVoid
              []
              []
          , Main []
          , Method
              (R.At
                 (R.Region
                    { R._start = R.Position {R._line = 5, R._column = 29}
                    , R._end = R.Position {R._line = 5, R._column = 37}
                    })
                 "aMethod2")
              TVoid
              [ R.At
                  (R.Region
                     { R._start = R.Position {R._line = 6, R._column = 52}
                     , R._end = R.Position {R._line = 6, R._column = 56}
                     })
                  (MethodArg "arg1")
              , R.At
                  (R.Region
                     { R._start = R.Position {R._line = 7, R._column = 52}
                     , R._end = R.Position {R._line = 7, R._column = 56}
                     })
                  (MethodArg "arg2")
              ]
              []
          ]
