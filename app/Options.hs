{-# LANGUAGE LambdaCase #-}

module Options
  ( Options (..)
  , OutputFormat (..)
  , optionsInfo
  ) where

import           Options.Applicative (Parser, ParserInfo, argument,
                                      eitherReader, fullDesc, header, help,
                                      helper, info, long, metavar, option,
                                      progDesc, short, switch, value)
import           Text.Megaparsec     (parseMaybe)

import           CNF
import qualified Parser

-- The options.
data Options = Options
  { formula      :: CNF Conjunction
  , listSteps    :: Bool
  , outputFormat :: OutputFormat
  , showSolution :: Bool
  }

-- The output format.
data OutputFormat = Text | Latex

-- The options parser.
options :: Parser Options
options = Options <$> _formula
                  <*> _listSteps
                  <*> _outputFormat
                  <*> _showSolution where
  _formula = argument parseFormula $
       metavar "FORMULA"
    where
      parseFormula = eitherReader $
        \ s -> case parseMaybe Parser.formula s of
          Nothing -> Left "parse error."
          Just f  -> Right f
  _listSteps = switch $
       long "list-steps"
    <> short 'l'
    <> help "List the steps of the DPLL algorithm execution."
  _outputFormat = option parseOutputFormat $
       long "output-format"
    <> short 'o'
    <> metavar "FORMAT"
    <> value Text
    <> help ( "The output format.  Currently supported: text and latex.  " ++
              "Defaults to text." )
    where
      parseOutputFormat = eitherReader $ \ case
        "text"  -> Right Text
        "latex" -> Right Latex
        fmt     -> Left $ "unkown output format: " ++ fmt ++ "."
  _showSolution = switch $
       long "show-solution"
    <> short 's'
    <> help "Show a solution to the formula, if satisfiable."

-- The options parser with the global program info and description.
optionsInfo :: ParserInfo Options
optionsInfo = info (helper <*> options) $
     fullDesc
  <> progDesc "Determine the satisfiability of the FORMULA."
  <> header "dpll - a SAT solver"
