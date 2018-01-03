{-# LANGUAGE TupleSections #-}

module Language.PHP.Parser where

-- Reference: https://github.com/php/php-langspec/tree/master/spec

import Data.Bifunctor
import Data.Void
import Control.Monad (guard)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Expr as Expr
import qualified Text.Megaparsec.Char.Lexer as Lex

import Language.PHP.AST
import Language.PHP.AST.Ops

-- TODO: Not sure if this is necessary, but at least put it somewhere else.
newtype LiteralShow = Lit String
    deriving (Eq)

instance Show LiteralShow where
    show (Lit s) = s

parsePretty :: String -> Either LiteralShow Expr
parsePretty = first (Lit . parseErrorPretty) . runParser expr ""

type Parser = Parsec Void String

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

comma :: Parser String
comma = symbol ","

semicolon :: Parser String
semicolon = symbol ";"

dollar :: Parser String
dollar = symbol "$"

surround :: String -> String -> Parser a -> Parser a
surround begin end = between (symbol begin) (symbol end)

parens :: Parser a -> Parser a
parens = surround "(" ")"

brackets :: Parser a -> Parser a
brackets = surround "[" "]"

braces :: Parser a -> Parser a
braces = surround "{" "}"

-- * Literals

-- | Parse a literal value, i.e. a string, a bool, etc.
literal :: Parser Literal
literal = choice
    -- We use 'try' here because we might have to backtrack if a string of numbers
    -- turns out to be an integer, not a float.
    -- TODO: left-factor this maybe?
    [ try $ Float <$> float
    , Int <$> int
    , Bool <$> bool
    , SingleQuotes <$> singleQuotes
    , DoubleQuotes <$> doubleQuotes
    , Backticks <$> backticks
    , Null <$ null'
    ]

int :: Parser Word
int = lexeme $ choice
    [ try $ char '0' *> (Lex.octal <|> char' 'x' *> Lex.hexadecimal)
    , Lex.decimal
    -- TODO: binary literals
    -- char '0' *> char 'b' *> some (satisfy (`member` "01"))
    -- ... and then convert that to an int
    ]

float :: Parser Float
-- TODO: check if @Lex.float@ matches completely with PHP's grammar for floats.
float = lexeme $ Lex.float

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

-- * Identifiers & Keywords

ident :: Parser Ident
ident = lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "identifier"

varIdent :: Parser Ident
varIdent = char '$' *> ident

-- * Expressions

term :: Parser Expr
term = choice
    [ parens expr
    , try $ (\(fx, d, v) -> IncDec fx d v) <$> incDec
    , Var <$> var <?> "variable"
    , Literal <$> literal <?> "literal value"
    , Const <$> ident <?> "constant"
    ]

-- TODO: All that complicated stuff with namespaces and class members.
var :: Parser Var
var = choice
    [ try $ SimpleVar <$> varIdent
    , try $ VarVar <$> (dollar *> var)
    , ExprVar <$> (dollar *> braces expr)
    ]

incDec :: Parser (Fixity, Delta, Var)
incDec = choice
    [ (Prefix, Increment, ) <$> (symbol "++" *> var)
    , (Prefix, Decrement, ) <$> (symbol "--" *> var)
    , try $ (Postfix, Increment, ) <$> (var <* symbol "++")
    , (Postfix, Decrement, ) <$> (var <* symbol "--")
    ]

expr :: Parser Expr
expr = lowerOpExpr

-- | Parse an operator with lower precedence than assignment.
-- |
-- | Specifically, these are the @and@, @xor@, and @or@ operators.
lowerOpExpr :: Parser Expr
lowerOpExpr = Expr.makeExprParser assignmentExpr $ makeOps lowerOperators

assignmentExpr :: Parser Expr
assignmentExpr = choice
    [ try $ Assignment <$> assignment
    , conditionalExpr
    ]

-- TODO: Error messages are a bit weird here, probably due to backtracking.
assignment :: Parser Assignment
assignment = do
    lhs <- var
    op <- assignOp
    choice
        [ try $ do
            guard (op == Assign)
            ByRef lhs <$> (symbol "&" *> var)
        , ByValue op lhs <$> assignmentExpr
        ]
    where
    assignOp = choice $ fmap (\(op, sym) -> op <$ symbol sym) assignOps

-- | Parse a conditional expression, i.e. the @:?@ operator.
conditionalExpr :: Parser Expr
conditionalExpr = Expr.makeExprParser higherOpExpr [[ Expr.InfixL middle ]]
    where
    -- The ternary operator in this case can be seen as an ordinary
    -- left-associative binary operator, except that the operator itself can
    -- contain an expression in the middle. So we can just let 'makeExprParser'
    -- do all the heavy lifting.
    middle = do
        symbol "?"
        t <- optional expr
        symbol ":"
        pure $ \c f -> Conditional c t f

-- | Parse an operator with a higher precedence than @:?@ and assignment.
higherOpExpr :: Parser Expr
higherOpExpr = Expr.makeExprParser term $ makeOps higherOperators

makeOps :: OperatorTable -> [[Expr.Operator Parser Expr]]
makeOps = fmap $ \(fx, ops) -> fmap (makeOperator fx) ops
    where
    -- TODO: Make this whole thing less messy
    makeOperator fixity (op, sym) =
        case fixity of
            InfixLeft -> Expr.InfixL $ binOp op sym
            InfixRight -> Expr.InfixR $ binOp op sym
            InfixNone -> Expr.InfixN $ binOp op sym
            Prefix -> Expr.Prefix $ unOp op sym
            Postfix -> Expr.Postfix $ unOp op sym
    binOp op sym = (BinOp $ MkBinOp $ op) <$ trySym sym
    unOp op sym = (UnOp $ MkUnOp $ op) <$ symbol sym
    trySym s = lexeme $ try
        $ string s <* notFollowedBy punctuationChar
