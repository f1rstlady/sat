{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module DPLL
  ( Selector (..)
  , Step (..)
  , Log (..)
  , Transformation
  , dpll
  , Solution (..)
  , solution
  , satisfiable
  ) where

import           Control.Monad        (foldM, (<=<), (>=>))
import           Control.Monad.Except (ExceptT, liftEither, runExceptT)
import           Control.Monad.Writer (Writer, runWriter, tell)
import           Data.Function        ((&))
import           Data.Maybe           (fromMaybe)
import           Prelude              hiding (log)

import           CNF                  hiding (eliminate, propagate)
import qualified CNF
import           Printer.Util

-- The representation of the selectors used for elimination.
data Selector = Units | PureLiterals

-- The selector associated with the representation.
selector :: Selector -> CNF Conjunction -> [CNF Literal]
selector Units        = units
selector PureLiterals = pureLiterals

-- The representation of the steps taken during the DPLL algorithm.
data Step = Propagate (CNF Literal) (Either Bool (CNF Conjunction))
          | Eliminate Selector [CNF Literal]
          | Branch String Log (Maybe Log)

-- The log is a list of steps.
newtype Log = Log { steps :: [Step] }
  deriving (Semigroup, Monoid)

-- For showing the log, print each step on a separate line instead of showing it
-- like a usual list.
instance Show Log where
  show = unlines' . map show . steps

-- To a selector, associate its corresponding name.  The name is given in
-- singular form to let the caller decide whether it has to be shown in singular
-- or plural form.
instance Show Selector where
  show Units        = "unit"
  show PureLiterals = "pure literal"

-- Show a step
instance Show Step where
  show (Propagate l f) =
    "Propagate " ++ show l ++ ": " ++ either show show f ++ "."
  show (Eliminate s ls) =
    "Eliminate the " ++ show s ++ pluralS ls
      ++ " " ++ enumerate (show <$> ls) ++ "."
  show (Branch x pos mNeg) = unlines' $
    [ "Branch on the variable " ++ x ++ "."]
      ++ showBranch (Pos x) pos
      ++ maybe [] (showBranch (Neg x)) mNeg
    where
      showBranch :: CNF Literal -> Log -> [String]
      showBranch l log =
        ( " " ++ (case l of Pos _ -> "1"; Neg _ -> "2")
          ++ ". Assume " ++ show l ++ " holds.")
        -- Indent the steps taken in the branch.
        : (map (indent 4) . lines $ show log)

-- A transformation is a writer monad logging the intermediate results and
-- yielding the transformed formula.  Eventually, the formula is true or false,
-- hence the exception trait `ExceptT Bool` is added.
type Transformation = ExceptT Bool (Writer Log) (CNF Conjunction)

-- Propagate the literal in the formula and simplify accordingly.  Additionally,
-- log the propagated literal and the simplified formula.
propagate :: CNF Literal -> CNF Conjunction -> Transformation
propagate l f = do
  let g = CNF.propagate l f
  tell $ Log [Propagate l g]
  liftEither g

-- Iteratively eliminate the literals yielded by the selector.  Additionally,
-- log the selector and the selected literals.
eliminate :: Selector -> CNF Conjunction -> Transformation
eliminate sel f =
  case selector sel f of
    [] -> liftEither (Right f)
    ls -> do
      tell $ Log [Eliminate sel ls]
      foldM (&) f (propagate <$> ls) >>= eliminate sel

-- Branch on a random literal.  Additionally, log the variable that is branched
-- on and the log of the DPLL algorithm of each branch.  The log for second
-- branch is only added if the first branch was not satisfiable.
branch :: CNF Conjunction -> Writer Log Bool
branch f = do
  let x = head (variables f)
      [(satPos, logPos), (satNeg, logNeg)] =
        runWriter . (either return dpll <=< runExceptT) . ($ f) . propagate <$>
          [Pos x, Neg x]
  tell $ Log [Branch x logPos (if satPos then Nothing else Just logNeg)]
  return (satPos || satNeg)

-- The DPLL algorithm, with its steps contained in the writer monad.
dpll :: CNF Conjunction -> Writer Log Bool
dpll =
  runExceptT . (eliminate Units >=> eliminate PureLiterals)
    >=> either return branch

-- The representation of a solution.
newtype Solution = Solution [CNF Literal]

-- For showing a solution, enumerate its literals.
instance Show Solution where
  show (Solution ls) = enumerate (show <$> ls)

-- From the log of the DPLL algorithm applied on a satisfiable formula, extract
-- the found solution.
solution :: Log -> Solution
solution = Solution . foldr extractPropagations [] . steps where
  extractPropagations (Propagate l _) = (l :)
  extractPropagations (Eliminate _ _) = id
  extractPropagations (Branch _ left mRight) = (ls ++) where
    (Solution ls) = solution (fromMaybe left mRight)

-- Whether the formula is satisfiable.
satisfiable :: CNF Conjunction -> Bool
satisfiable = fst . runWriter . dpll
