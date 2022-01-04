{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module DPLL
  ( Selector (..)
  , Step (..)
  , Log (..)
  , Transformation
  , dpll
  , satisfiable
  ) where

import           Control.Monad        (foldM, (<=<), (>=>))
import           Control.Monad.Except (ExceptT, liftEither, runExceptT)
import           Control.Monad.Writer (Writer, runWriter, tell)
import           Data.Function        ((&))
import           Data.List            (intercalate)
import           Prelude              hiding (log, unlines)

import           CNF                  (CNF (..), Conjunction, Literal,
                                       identifier, pureLiterals, units,
                                       variables)
import qualified CNF                  (propagate)

data Selector = Units | PureLiterals

selector :: Selector -> CNF Conjunction -> [CNF Literal]
selector Units        = units
selector PureLiterals = pureLiterals

data Step = Propagate (CNF Literal) (Either Bool (CNF Conjunction))
          | Eliminate Selector [CNF Literal]
          | Branch String Log (Maybe Log)

unlines :: [String] -> String
unlines = intercalate "\n"

newtype Log = Log { steps :: [Step] }
  deriving (Semigroup, Monoid)

instance Show Log where
  show = unlines . map show . steps

instance Show Selector where
  show Units        = "unit"
  show PureLiterals = "pure literal"

instance Show Step where
  show (Propagate l f) =
    "Propagate " ++ show l ++ ": " ++ either show show f ++ "."
  show (Eliminate s ls) =
    let sel = show s ++ (case ls of [_] -> ""; _   -> "s")
     in "Eliminate the " ++ sel ++ " " ++ intercalate ", " (map show ls) ++ "."
  show (Branch x pos mNeg) = unlines $
    [ "Branch on the variable " ++ x ++ "."]
      ++ showBranch (Pos x) pos
      ++ maybe [] (showBranch (Neg x)) mNeg
    where
      showBranch :: CNF Literal -> Log -> [String]
      showBranch l log =
        ( " " ++ (case l of Pos _ -> "1"; Neg _ -> "2")
          ++ ". Assume " ++ identifier l ++ " holds.")
        : (map ("    " ++) . lines $ show log)

type Transformation = ExceptT Bool (Writer Log) (CNF Conjunction)

propagate :: CNF Literal -> CNF Conjunction -> Transformation
propagate l f = do
  let g = CNF.propagate l f
  tell $ Log [Propagate l g]
  liftEither g

-- Iteratively eliminate the literals yielded by the selector.
eliminate :: Selector -> CNF Conjunction -> Transformation
eliminate sel f =
  case selector sel f of
    [] -> liftEither (Right f)
    ls -> do
      tell $ Log [Eliminate sel ls]
      foldM (&) f (propagate <$> ls) >>= eliminate sel

-- Branch on a random literal
branch :: CNF Conjunction -> Writer Log Bool
branch f = do
  let x = head (variables f)
      [(satPos, logPos), (satNeg, logNeg)] =
        runWriter . (either return dpll <=< runExceptT) . ($ f) . propagate <$>
          [Pos x, Neg x]
  tell $ Log [Branch x logPos (if satPos then Nothing else Just logNeg)]
  return (satPos || satNeg)

-- The DPLL algorithm.
dpll :: CNF Conjunction -> Writer Log Bool
dpll =
  runExceptT . (eliminate Units >=> eliminate PureLiterals)
    >=> either return branch

-- Whether the formula is satisfiable
satisfiable :: CNF Conjunction -> Bool
satisfiable = fst . runWriter . dpll
