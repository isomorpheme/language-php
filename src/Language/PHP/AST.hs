module Language.PHP.AST where

data Token
    = TokBinOp BinOp
    | TokUnOp UnOp
    | TokLiteral Literal
    | Comma
    | Semicolon
    | LeftParen | RightParen
    | LeftBracket | RightBracket
    | LeftBrace | RightBrace
    | Keyword Keyword
    | Dollar
    | Ident Ident
    deriving (Show)

type Keyword = String
type Ident = String

data Literal
    = Int Int -- TODO: split into decimal, hexadecimal, etc.
    | Float Float
    | Bool Bool
    | SingleQuotes String
    | DoubleQuotes String
    -- | HereDoc | NowDoc -- Not even gonna bother for now.
    | Backticks String
    | Null
    deriving (Show)

data BinOp
    -- Arithmetic
    = Add -- +
    | Subtract -- -
    | Multiply -- *
    | Divide -- /
    | Modulo -- %
    | Exponentiate -- **

    -- Logical
    | And -- &&
    | Or -- ||
    | Xor -- xor
    | AndAlt -- and
    | OrAlt -- or

    -- Comparison
    | Equal -- ==
    | Identical -- ===
    | NotEqual -- !=
    | NotEqualAlt -- <> -- Because SQL or something.
    | NotIdentical -- !==
    | LessThan -- <
    | GreaterThan -- >
    | LessThanEqual -- <=
    | GreaterThanEqual -- >=
    -- | Spaceship -- <=> apparently, and only in PHP 7.
    -- These people were smoking some good stuff when they came up with this shit.

    -- Bitwise
    | BitwiseAnd -- &
    | BitwiseOr -- |
    | BitwiseXor -- ^
    | ShiftLeft -- <<
    | ShiftRight -- >>

    -- String Manipulation
    | Concat -- .

    -- Assignment
    | Assign -- =
    | AddAssign -- +=
    | SubtractAssign -- -=
    | MultiplyAssign -- *=
    | DivideAssign -- /=
    | ModuloAssign -- %=
    | ConcatAssign -- .=
    | BWAndAssign -- &=
    | BWOrAssign -- |=
    | BWXorAssign -- ^=
    | LeftShiftAssign -- <<=
    | RightShiftAssign -- >>=

    -- Types
    | InstanceOf -- instanceof
    deriving (Show)

data UnOp
    -- Arithmetic
    = Identity -- +
    | Negate -- -

    -- Impure Arithmetic
    | Increment -- ++
    | Decrement -- --

    -- Logical
    | Not -- !

    -- Bitwise
    | BitwiseNot -- ~

    -- Error Control
    | Ignore -- @
    -- Because why not make `try { foo(); } catch () {}` an operator...
    deriving (Show)

data Var
    = SimpleVar Ident
    | VarVar Var -- Please don't, though.
    | ExprVar Expr -- *Especially* don't.
    deriving (Show)

data Expr
    -- Values
    = Var Var
    | Literal Literal
    | Const Ident

    -- Operators
    | BinOp BinOp Expr Expr
    | UnOp UnOp Expr

    -- Compound
    | Conditional Expr Expr Expr
    | FunctionCall Expr [Expr]
    | MethodCall Expr Expr [Expr]

    -- Yes, these are actually expressions.
    | Require Expr
    | Include Expr
    | RequireOnce Expr
    | IncludeOnce Expr
    deriving (Show)

data Block
    = Many Stmts
    | Single Stmt
    deriving (Show)

type Stmts = [Stmt]

data Stmt
    = Expr Expr

    -- Control Flow
    | If Expr Block (Maybe Block) (Maybe [(Expr, Block)])
    | While Expr Block
    | DoWhile Block Expr
    | For Expr Expr Expr Block
    | ForEach Expr Expr (Maybe Expr) Block
    | Break (Maybe Expr)
    | Continue (Maybe Expr)
    | Switch Expr [(Expr, Stmts)]
    | Return Expr
    | Goto Ident -- More restricted than C, but still considered Harmful as Fuck.
    | Label Ident

    -- Declarations
    | Declare Ident Literal (Maybe Stmts) -- Pragmas, but for PHP and also useless.
    | Global [Var]
    deriving (Show)
