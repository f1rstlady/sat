{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE EmptyDataDeriving  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main
  ( Literal
  , Disjunction
  , Conjunction
  , CNF (..)
  , identifier
  , variables
  , units
  , pureLiterals
  , Selector (..)
  , selector
  , Step (..)
  , Log
  , Transformation
  , propagate
  , propagateAndLog
  , eliminate
  , dpll
  , main) where

import Control.Monad             (foldM, (>=>), (<=<))
import Control.Monad.Except      (ExceptT, liftEither, runExceptT)
import Control.Monad.Writer      (Writer, runWriter, tell)
import Data.Function             ((&))
import Data.List                 (intercalate)
import Data.Map                  (Map, elems, empty, insertWith)
import Data.Maybe                (catMaybes)

-- Types that indicate the part of a conjunctive normal form
data Literal deriving (Eq)
data Disjunction deriving (Eq)
data Conjunction deriving (Eq)

-- Constructors for a conjunctive normal form
data CNF a where
  Pos :: String -> CNF Literal
  Neg :: String -> CNF Literal
  Or  :: [CNF Literal] -> CNF Disjunction
  And :: [CNF Disjunction] -> CNF Conjunction

deriving instance Eq a => Eq (CNF a)

-- Custom show instance to use mathematical notation.
instance Show (CNF a) where
  show (Pos x) = x
  show (Neg x) = "¬" ++ x
  show (Or ls) =
    case ls of
      []  -> "⊥"
      [k] -> show k
      ks  -> "(" ++ intercalate " ∨ " (map show ks) ++ ")"
  show (And ds) =
    case ds of
      [] -> "⊤"
      es -> intercalate " ∧ " (map show es)

-- Get the identifier of a literal.
identifier :: CNF Literal -> String
identifier (Pos x) = x
identifier (Neg x) = x

-- Get the variables of a formula.
variables :: CNF a -> [String]
variables (Pos x)  = [x]
variables (Neg x)  = [x]
variables (Or ls)  = ls >>= variables
variables (And ds) = ds >>= variables

-- Get the units of a disjunction.
units :: CNF Conjunction -> [CNF Literal]
units (And ds) = foldr appendIfUnit [] ds where
  appendIfUnit :: CNF Disjunction -> [CNF Literal] -> [CNF Literal]
  appendIfUnit (Or [l]) = (l :)
  appendIfUnit _        = id

-- Get the pure literals of a formula.
pureLiterals :: CNF a -> [CNF Literal]
pureLiterals = catMaybes . elems . flip pureLiterals' empty

type PurityMap = Map String (Maybe (CNF Literal))

pureLiterals' :: CNF a -> PurityMap -> PurityMap
pureLiterals' f m =
  case f of
    l@(Pos _) -> insertLiteral l
    l@(Neg _) -> insertLiteral l
    Or ls     -> foldr pureLiterals' m ls
    And ds    -> foldr pureLiterals' m ds
  where
    insertLiteral l = insertWith checkConflict (identifier l) (Just l) m where
      checkConflict _        Nothing  = Nothing
      checkConflict (Just l) (Just k) = if k == l then Just k else Nothing
      checkConflict Nothing  (Just _) = error "Trying to update nothing!"

data Selector = Units | PureLiterals

selector :: Selector -> CNF Conjunction -> [CNF Literal]
selector Units        = units
selector PureLiterals = pureLiterals

data Step = Propagate (CNF Literal) (Either Bool (CNF Conjunction))
          | Eliminate Selector [CNF Literal]
          | Branch String Log (Maybe Log)

type Log = [Step]

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
      unlines = intercalate "\n"
      showBranch :: CNF Literal -> Log -> [String]
      showBranch l steps =
        let indent = unlines . map ("    " ++) . lines
         in [ " " ++ (case l of Pos _ -> "1"; Neg _ -> "2")
              ++ ". Assume " ++ identifier l ++ " holds."
            , unlines $ map (indent . show) steps
            ]

type Transformation = ExceptT Bool (Writer Log) (CNF Conjunction)

-- Propagate a literal in a formula and simplify it accordingly.
propagate :: CNF Literal -> CNF t -> Either Bool (CNF t)
propagate (Pos x) l@(Pos y) = if x == y then Left True  else Right l
propagate (Neg x) l@(Pos y) = if x == y then Left False else Right l
propagate (Pos x) l@(Neg y) = if x == y then Left False else Right l
propagate (Neg x) l@(Neg y) = if x == y then Left True  else Right l
propagate x (Or ls) = foldr simplify (Left False) ls where
  simplify _ (Left True) = Left True
  simplify l acc =
    case propagate x l of
      Left True  -> Left True
      Left False -> acc
      Right k    -> Right . Or $
        case acc of
          Right (Or ks) -> k : ks
          _             -> [k]
propagate x (And ds) = foldr simplify (Left True) ds where
  simplify _ (Left False) = Left False
  simplify d acc =
    case propagate x d of
      Left True  -> acc
      Left False -> Left False
      Right e    -> Right . And $
        case acc of
          Right (And es) -> e : es
          _              -> [e]

propagateAndLog :: CNF Literal -> CNF Conjunction -> Transformation
propagateAndLog l f = do
  let g = propagate l f
  tell [Propagate l g]
  liftEither g

-- Iteratively eliminate the literals yielded by the selector.
eliminate :: Selector -> CNF Conjunction -> Transformation
eliminate sel f =
  case selector sel f of
    [] -> liftEither (Right f)
    ls -> do
      tell [Eliminate sel ls]
      foldM (&) f (propagateAndLog <$> ls) >>= eliminate sel

-- Branch on a random literal
branch :: CNF Conjunction -> Writer Log Bool
branch f = do
  let x = head (variables f)
      [(satPos, logPos), (satNeg, logNeg)] =
        map (runWriter . (either return dpll <=< runExceptT) . ($ f) . propagateAndLog)
            [Pos x, Neg x]
  tell [Branch x logPos (if satPos then Nothing else Just logNeg)]
  return (satPos || satNeg)

-- The DPLL algorithm.
dpll :: CNF Conjunction -> Writer Log Bool
dpll =
  runExceptT . (eliminate Units >=> eliminate PureLiterals)
    >=> either return branch

-- Whether the formula is satisfiable
satisfiable :: CNF Conjunction -> Bool
satisfiable = fst . runWriter . dpll

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
