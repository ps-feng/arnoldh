module Region where

data Region = Region
  { _start :: Position
  , _end :: Position
  } deriving (Eq, Show, Ord)

data Position = Position
  { _line :: Int
  , _column :: Int
  } deriving (Eq, Show, Ord)

data Located a =
  At Region
     a
  deriving (Eq, Show, Ord)

instance Functor Located where
  fmap f val = locate (f $ unlocate val) val

at :: Position -> Position -> a -> Located a
at start end value = At (Region start end) value

unlocate :: Located a -> a
unlocate (At _ a) = a

locate :: a -> Located b -> Located a
locate a (At region _) = At region a

getLocation :: Located a -> Region
getLocation (At region _) = region
