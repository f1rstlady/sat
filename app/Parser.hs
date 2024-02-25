module Parser (
  formula,
) where

import CNF hiding (identifier)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

-- An identifier consists of lettters.
identifier :: Parser String
identifier = label "identifier" $ lexeme $ some letterChar

-- posLiteral := identifier
posLiteral :: Parser (CNF Literal)
posLiteral = label "positive literal" $ lexeme $ Pos <$> identifier

-- posLiteral := `¬` identifier
negLiteral :: Parser (CNF Literal)
negLiteral =
  label "negative literal" $ lexeme $ symbol "¬" *> (Neg <$> identifier)

-- literal := posLiteral | negLiteral
literal :: Parser (CNF Literal)
literal = label "literal" $ lexeme $ posLiteral <|> negLiteral

-- disjunction := literal | `(` literal ( `∨` literal )+ `)`
disjunction :: Parser (CNF Disjunction)
disjunction =
  label "disjunction" $
    lexeme $
      try oneLiteral <|> between (symbol "(") (symbol ")") moreLiterals
 where
  oneLiteral = Or . (: []) <$> literal
  moreLiterals =
    (\l ls -> Or (l : ls))
      <$> literal
      <*> some (try $ symbol "∨" *> literal)

-- conjunction := disjunction ( `∧` disjunction )*
conjunction :: Parser (CNF Conjunction)
conjunction =
  label "conjunction" $
    lexeme $
      (\d ds -> And (d : ds))
        <$> disjunction
        <*> many (try $ symbol "∧" *> disjunction)

-- formula := conjunction
formula :: Parser (CNF Conjunction)
formula = label "formula" $ space *> conjunction <* eof
