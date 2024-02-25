module Main (
  main,
) where

import Control.Monad (when)
import Control.Monad.Writer (runWriter)
import DPLL
import Options
import Options.Applicative (execParser)
import Printer.Latex
import Prelude hiding (log)

main :: IO ()
main = do
  opts <- execParser optionsInfo
  let format :: (Show a, ToLatex a) => a -> String
      format = case outputFormat opts of
        Text -> show
        Latex -> toLatex
      (sat, log) = runWriter . dpll $ formula opts
  when (listSteps opts) (putStrLn $ format log)
  if sat
    then do
      putStrLn "The formula is satisfiable."
      when (showSolution opts) $
        let sol = format (solution log)
         in putStrLn $ "A solution of the formula is " ++ sol ++ "."
    else putStrLn "The formula is not satisfiable."
