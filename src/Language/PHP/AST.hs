module Language.PHP.AST where

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

data UnOp
    -- Arithmetic
    = Identity -- +
    | Negate -- -

    -- Impure Arithmetic
    | PreIncrement -- ++
    | PostIncrement
    | PreDecrement -- --
    | PostDecrement

    -- Logical
    | Not -- !

    -- Bitwise
    | BitwiseNot -- ~

    -- Error Control
    | Ignore -- @
    -- Because why not make `try { foo(); } catch () {}` an operator...

type Ident = String

data Literal
    = Int Int
    | Float Float
    | Bool Bool
    | SingleQuotes String
    | DoubleQuotes String
    -- | HereDoc | NowDoc -- Not even gonna bother for now.
    | Backticks String
    | Null

data Var
    = SimpleVar Ident
    | VarVar Var -- Please don't, though.
    | ExprVar Expr -- *Especially* don't.

data Expr
    -- Values
    = Var Var
    | Literal Literal
    | Const Ident

    -- Operators
    | UnOp UnOp Expr
    | BinOp BinOp Expr Expr

    -- Compound
    | Conditional Expr Expr Expr
    | FunctionCall Expr [Expr]
    | MethodCall Expr Expr [Expr]

    -- Yes, these are actually expressions.
    | Require Expr
    | Include Expr
    | RequireOnce Expr
    | IncludeOnce Expr

data Block
    = Many Stmts
    | Single Stmt

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
