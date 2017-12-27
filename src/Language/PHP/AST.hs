module Language.PHP.AST where

import Language.PHP.AST.Ops

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

data Var
    = SimpleVar Ident
    | VarVar Var -- Please don't, though.
    | ExprVar Expr -- *Especially* don't.
    deriving (Show)

data Delta = Increment | Decrement
    deriving (Eq, Show)

data Assignment
    = ByValue AssignOp Var Expr
    | ByRef Var Var
    deriving (Show)

data Expr
    -- Values
    = Var Var
    | Literal Literal
    | Const Ident

    -- Operators
    | BinOp BinOp Expr Expr
    | UnOp UnOp Expr
    | IncDec Fixity Delta Var
    | Assignment Assignment

    -- Compound
    | Conditional Expr (Maybe Expr) Expr
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

{-
data Decl
    = Class
    | FunctionDecl MemberFlags Ident [Var] Stmts -- TODO: default args
    | PropertyDecl MemberFlags Ident Expr
    |
-}
