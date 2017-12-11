module Language.PHP.Lexer where

import Data.Void

import Text.Megaparsec hiding (Token, token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import Language.PHP.AST

type Parser = Parsec Void String

tokens :: Parser [Token]
tokens = many token <* eof

token :: Parser Token
token = choice
    [ TokLiteral <$> literal
    ]

spaceConsumer :: Parser ()
spaceConsumer = Lex.space space1 lineComment blockComment
    where
    lineComment = Lex.skipLineComment "//"
    blockComment = Lex.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = Lex.symbol spaceConsumer

symbol' :: String -> Parser String
symbol' = Lex.symbol' spaceConsumer

literal :: Parser Literal
literal = choice
    [ Float <$> try float
    , Int <$> int
    , Bool <$> bool
    , SingleQuotes <$> singleQuotes
    , DoubleQuotes <$> doubleQuotes
    , Backticks <$> backticks
    , Null <$ null'
    ]

int :: Parser Int
int = lexeme $ Lex.signed spaceConsumer $ choice
    [ char '0' *> (Lex.octal <|> char' 'x' *> Lex.hexadecimal)
    , Lex.decimal
    -- TODO: binary literals
    -- char '0' *> char 'b' *> some (satisfy (`member` "01"))
    -- ... and then convert that to an int
    ]

float :: Parser Float
-- TODO: check if @Lex.float@ matches completely with PHP's grammar for floats.
float = lexeme $ Lex.signed spaceConsumer $ Lex.float

bool :: Parser Bool
bool = True <$ symbol' "true"
    <|> False <$ symbol' "false"

quotes :: Char -> Parser String
quotes c = lexeme $ char c *> manyTill Lex.charLiteral (char c)

singleQuotes :: Parser String
singleQuotes = quotes '\''

doubleQuotes :: Parser String
doubleQuotes = quotes '"'

backticks :: Parser String
backticks = quotes '`'

null' :: Parser String
null' = symbol' "null"
