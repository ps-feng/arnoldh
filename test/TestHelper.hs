module TestHelper
  ( dummyRegion
  , errorRegion
  , located
  , locatedE
  ) where

import qualified Region as R

dummyRegion :: R.Region
dummyRegion =
  R.Region
    { R._start = R.Position {R._line = 1, R._column = 1}
    , R._end = R.Position {R._line = 1, R._column = 2}
    }

errorRegion :: R.Region
errorRegion =
  R.Region
    { R._start = R.Position {R._line = 5, R._column = 38}
    , R._end = R.Position {R._line = 5, R._column = 40}
    }

located :: a -> R.Located a
located a = R.At dummyRegion a

locatedE :: a -> R.Located a
locatedE a = R.At errorRegion a
