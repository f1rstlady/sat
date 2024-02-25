-- A parser for CNF formulae.  It is space-insensitive.
module Parser (
  formula,
) where

import CNF hiding (identifier)
import Data.Void (Void)
import Text.Megaparsec (
  Parsec,
  between,
  eof,
  many,
  some,
  try,
  (<?>),
  (<|>),
 )
import Text.Megaparsec.Char (char, letterChar, space)

type Parser = Parsec Void String

-- An identifier consists of lettters.
identifier :: Parser String
identifier =
  some letterChar
    <?> "identifier"

-- posLiteral := identifier
posLiteral :: Parser (CNF Literal)
posLiteral =
  Pos <$> identifier
    <?> "positive literal"

-- posLiteral := `¬` identifier
negLiteral :: Parser (CNF Literal)
negLiteral =
  char '¬' *> space *> (Neg <$> identifier)
    <?> "negative literal"

-- literal := posLiteral | negLiteral
literal :: Parser (CNF Literal)
literal =
  posLiteral <|> negLiteral
    <?> "literal"

-- disjunction := literal | `(` literal ( `∨` literal )+ `)`
disjunction :: Parser (CNF Disjunction)
disjunction =
  (try oneLiteral <|> between (char '(') (char ')') moreLiterals)
    <?> "disjunction"
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
  (\d ds -> And (d : ds))
    <$> disjunction
    <*> many (try $ space *> char '∧' *> space *> disjunction)
    <?> "conjunction"

-- formula := conjunction
formula :: Parser (CNF Conjunction)
formula = space *> conjunction <* space <* eof
