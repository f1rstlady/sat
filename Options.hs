module Options
  ( Options (..)
  , optionsInfo
  ) where

import           Options.Applicative (Parser, ParserInfo, fullDesc, header,
                                      help, helper, info, long, progDesc, short,
                                      switch)

data Options = Options
  { listSteps    :: Bool
  , showSolution :: Bool
  }

options :: Parser Options
options = Options <$> _listSteps <*> _showSolution where
  _listSteps = switch $
       long "list-steps"
    <> short 'l'
    <> help "List the steps of the DPLL algorithm execution."
  _showSolution = switch $
       long "show-solution"
    <> short 's'
    <> help "Show a solution to the formula, if satisfiable."

optionsInfo :: ParserInfo Options
optionsInfo = info (helper <*> options) $
     fullDesc
  <> progDesc "Determine the satisfiability of the FORMULA."
  <> header "dpll - a SAT solver"
