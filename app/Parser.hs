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

-- literal := posLiteral | negLiteral
literal :: Parser (CNF Literal)
literal = label "literal" $ posLiteral <|> negLiteral
 where
  posLiteral = Pos <$> identifier
  negLiteral = Neg <$> (symbol "¬" *> identifier)

-- disjunction := literal | `(` literal ( `∨` literal )+ `)`
disjunction :: Parser (CNF Disjunction)
disjunction = label "disjunction" $ try oneLiteral <|> moreLiterals
 where
  oneLiteral = Or . (: []) <$> literal
  moreLiterals =
    Or <$> between (symbol "(") (symbol ")") (sepBy1 literal $ symbol "∨")

-- conjunction := disjunction ( `∧` disjunction )*
conjunction :: Parser (CNF Conjunction)
conjunction = label "conjunction" $ And <$> sepBy1 disjunction (symbol "∧")

-- formula := conjunction
formula :: Parser (CNF Conjunction)
formula = label "formula" $ space *> conjunction <* eof
