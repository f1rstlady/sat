module Main
  ( main
  ) where

import           Control.Monad.Writer (runWriter)

import           CNF                  (CNF (..), Conjunction)
import           DPLL                 (dpll)

main :: IO ()
main = mapM_ print $ snd . runWriter $ dpll f1

f1 :: CNF Conjunction
f1 = And
  [ Or [Neg "X", Neg "Y", Pos "Z"]
  , Or [Pos "X", Pos "Z", Pos "W"]
  , Or [Pos "X", Pos "Z", Neg "W"]
  , Or [Pos "X", Neg "Z", Pos "W"]
  , Or [Pos "X", Neg "Z", Neg "W"]
  , Or [Pos "Y", Neg "Z", Pos "W"]
  , Or [Neg "X", Neg "Y", Neg "Z"]
  , Or [Neg "X", Pos "Y", Pos "Z"]
  ]
