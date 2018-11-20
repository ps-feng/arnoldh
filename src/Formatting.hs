module Formatting
  ( caret
  , indent
  , indent2
  , indent4
  , eol
  , eol1
  , eol2
  ) where

caret :: Int -> String
caret n = replicate n '^'

indent :: Int -> String
indent n = replicate n ' '

indent2 :: String
indent2 = indent 2

indent4 :: String
indent4 = indent 4

eol :: Int -> String
eol n = replicate n '\n'

eol1 :: String
eol1 = eol 1

eol2 :: String
eol2 = eol 2
