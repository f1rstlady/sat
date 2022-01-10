{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE EmptyDataDeriving  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module CNF
  ( Literal
  , Disjunction
  , Conjunction
  , CNF (..)
  , identifier
  , variables
  , units
  , pureLiterals
  , propagate
  , eliminate
  ) where

import           Control.Monad (foldM)
import           Data.Function ((&))
import           Data.List     (intercalate)
import           Data.Map      (Map, elems, empty, insertWith)
import           Data.Maybe    (catMaybes)

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
      _   -> "(" ++ intercalate " ∨ " (map show ls) ++ ")"
  show (And ds) =
    case ds of
      [] -> "⊤"
      _  -> intercalate " ∧ " (map show ds)

-- Get the identifier of the literal.
identifier :: CNF Literal -> String
identifier (Pos x) = x
identifier (Neg x) = x

-- Get the variables of the formula.
variables :: CNF a -> [String]
variables (Pos x)  = [x]
variables (Neg x)  = [x]
variables (Or ls)  = ls >>= variables
variables (And ds) = ds >>= variables

-- Get the units of the disjunction.
units :: CNF Conjunction -> [CNF Literal]
units (And ds) = foldr appendIfUnit [] ds where
  appendIfUnit :: CNF Disjunction -> [CNF Literal] -> [CNF Literal]
  appendIfUnit (Or [l]) = (l :)
  appendIfUnit _        = id

-- Get the pure literals of the formula.
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
      checkConflict _        Nothing   = Nothing
      checkConflict (Just k) (Just k') = if k == k' then Just k' else Nothing
      checkConflict Nothing  (Just _)  = error "Trying to update nothing!"

-- Propagate the literal in the formula and simplify accordingly.
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

-- Iteratively eliminate the literals yielded by the selector.
eliminate :: (CNF t -> [CNF Literal]) -> CNF t -> Either Bool (CNF t)
eliminate sel f =
  case sel f of
    [] -> Right f
    ls -> foldM (&) f (propagate <$> ls) >>= eliminate sel
