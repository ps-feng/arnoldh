module Region where

data Region = Region
  { start :: Position
  , end :: Position
  }

data Position = Position
  { _line :: Int
  , _column :: Int
  }

data Located a =
  At Region
     a
