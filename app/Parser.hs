-- A parser for CNF formulae.  It is space-insensitive.
module Parser (
  formula,
) where

import CNF hiding (identifier)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

-- An identifier consists of lettters.
identifier :: Parser String
identifier =
  label "identifier" $
    some letterChar

-- posLiteral := identifier
posLiteral :: Parser (CNF Literal)
posLiteral =
  label "positive literal" $
    Pos <$> identifier

-- posLiteral := `¬` identifier
negLiteral :: Parser (CNF Literal)
negLiteral =
  label "negative literal" $
    char '¬' *> space *> (Neg <$> identifier)

-- literal := posLiteral | negLiteral
literal :: Parser (CNF Literal)
literal =
  label "literal" $
    posLiteral <|> negLiteral

-- disjunction := literal | `(` literal ( `∨` literal )+ `)`
disjunction :: Parser (CNF Disjunction)
disjunction =
  label "disjunction" $
    try oneLiteral <|> between (char '(') (char ')') moreLiterals
 where
  oneLiteral = Or . (: []) <$> literal
  moreLiterals =
    (\l ls -> Or (l : ls))
      <$> (space *> literal)
      <*> some (try $ space *> char '∨' *> space *> literal)
      <* space

-- conjunction := disjunction ( `∧` disjunction )*
conjunction :: Parser (CNF Conjunction)
conjunction =
  label "conjunction" $
    (\d ds -> And (d : ds))
      <$> disjunction
      <*> many (try $ space *> char '∧' *> space *> disjunction)

-- formula := conjunction
formula :: Parser (CNF Conjunction)
formula =
  label "formula" $
    space *> conjunction <* space <* eof
