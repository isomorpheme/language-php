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
    , punctuation
    ]

spaceConsumer :: Parser ()
spaceConsumer = Lex.space space1 lineComment blockComment
    where
    lineComment = Lex.skipLineComment "//"
    blockComment = Lex.skipBlockComment "/*" "*/"

-- | Run a parser, and then consume all trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceConsumer

-- | Match a string, and then consume all trailing whitespace.
symbol :: String -> Parser String
symbol = Lex.symbol spaceConsumer

-- | Same as 'symbol', but case insensitive.
symbol' :: String -> Parser String
symbol' = Lex.symbol' spaceConsumer

-- * Punctuation

-- | Parse punctuation, i.e. @,@, @;@, @(@, etc.
punctuation :: Parser Token
punctuation = lexeme $ choice
    [ Comma <$ char ','
    , Semicolon <$ char ';'
    , LeftParen <$ char '(', RightParen <$ char ')'
    , LeftBracket <$ char '[', RightBracket <$ char ']'
    , LeftBrace <$ char '{', RightBrace <$ char '}'
    -- TODO: `$` shouln't *always* eat whitespace after it.
    -- `$ foo` should not parse, but `$ { 'foo' }` should. Yeah...
    , Dollar <$ char '$'
    ]

-- * Literals

-- | Parse a literal value, i.e. string, bool, etc.
literal :: Parser Literal
literal = choice
    -- We use 'try' here because we might have to backtrack if a string of numbers
    -- turns out to be an integer, not a float.
    -- TODO: left-factor this maybe?
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

-- | Given a character @c@, match @c@ followed by zero or more character literals
--   (which might include an escaped version of @c@), up until another ocurrence
--   of @c@.
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
