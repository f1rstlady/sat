{-# LANGUAGE GADTs #-}

module Printer.Latex
  ( ToLatex (..)
  ) where

import           Data.List    (intercalate)
import           Prelude      hiding (log)

import           CNF          (CNF (..), Literal)
import           DPLL         (Log, Step (..), steps)
import           Printer.Util (enumerate, indent, pluralS, unlines')

-- Generate code to print data with LaTeX.
class ToLatex a where
  toLatex :: a -> String

-- Standard shiftwidth for LaTeX code.
sw :: Int
sw = 4

-- Print CNF formulae to LaTeX.
instance ToLatex (CNF t) where
  toLatex (Pos x) = x
  toLatex (Neg x) = "\\neg " ++ x
  toLatex (Or ls) =
    case ls of
      []  -> "\\bot"
      [k] -> toLatex k
      _   -> "(" ++ intercalate " \\vee " (map toLatex ls) ++ ")"
  toLatex (And ds) =
    case ds of
      [] -> "\\top"
      es -> intercalate " \\wedge " (map toLatex es)

-- Set the formula in inline math.
inline :: CNF t -> String
inline f = "$" ++ toLatex f ++ "$"

-- Set the formula in display math.
display :: CNF t -> String
display f = "\\[ " ++ toLatex f ++ " \\]"

-- Print DPLL log to LaTeX.
instance ToLatex Step where
  toLatex (Propagate l f) = "Propagate " ++ inline l ++ ":" ++ printResult f
  toLatex (Eliminate s ls) =
    "Eliminate the " ++ show s ++ pluralS ls ++ " " ++ literals ++ "." where
      literals = enumerate (inline <$> ls)
  toLatex (Branch x pos mNeg) = unlines' $
    [ "Branch on the variable $" ++ x ++ "$."
    , "\\begin{enumerate}" ]
      ++ showBranch (Pos x) pos
      ++ maybe [] (showBranch (Neg x)) mNeg
      ++ [ "\\end{enumerate}" ]
    where
      showBranch :: CNF Literal -> Log -> [String]
      showBranch l log = map (indent sw) $
        ( "\\item Assume " ++ inline l ++ " holds." )
        -- Indent the steps taken in the branch.
        : (map (indent sw) . lines $ toLatex log)

instance ToLatex Log where
  toLatex = unlines' . map toLatex . steps

-- Print the intermediate result.
printResult :: Either Bool (CNF t) -> String
printResult (Left b)  = ' ' : show b ++ "."
printResult (Right f) = '\n' : indent sw (display f)
