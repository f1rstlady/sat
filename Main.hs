module Main
  ( main
  ) where

import           Control.Monad        (when)
import           Control.Monad.Writer (runWriter)
import           Options.Applicative  (execParser)
import           Prelude              hiding (log)

import           CNF
import           DPLL
import           Options
import           Printer.Util

main :: IO ()
main = do
  opts <- execParser optionsInfo
  let (sat, log) = runWriter $ dpll f1
  when (listSteps opts) (print log)
  if sat
     then do
       putStrLn "The formula is satisfiable."
       when (showSolution opts) $
         let sol = enumerate . map show $ solution log
         in putStrLn $ "A solution of the formula is " ++ sol ++ "."
     else putStrLn "The formula is not satisfiable."

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
