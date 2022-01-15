module Printer.Util
  ( unlines'
  , indent
  , pluralS
  , enumerate
  ) where

import           Data.List (intercalate)

-- A custom unlines function that omits the last '\n'.
unlines' :: [String] -> String
unlines' = intercalate "\n"

-- Indent the string by the given amount of whitespace.
indent :: Int -> String -> String
indent = (++) . (`replicate` ' ')

-- Print a plural `s` when the list has more than one element.
pluralS :: [a] -> String
pluralS []  = error "no element for reference!"
pluralS [_] = ""
pluralS _   = "s"

-- Enumerate a list of elements.
enumerate :: [String] -> String
enumerate []     = ""
enumerate [x]    = x
enumerate [x,y]  = x ++ " and " ++ y
enumerate (x:xs) = x ++ ", " ++ enumerate xs
