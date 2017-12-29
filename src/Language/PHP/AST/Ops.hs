module Language.PHP.AST.Ops where

import Data.List (union)
import Data.Semigroup ((<>))

data Op
    --- Binary Operators ---
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

    -- Coalesce
    -- | Coalesce -- ?? -- PHP 7 only

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

    -- Types
    | InstanceOf -- instanceof

    --- Unary Operators
    -- Arithmetic
    | Identity -- +
    | Negate -- -

    -- Logical
    | Not -- !

    -- Bitwise
    | BitwiseNot -- ~

    -- Error Control
    | Ignore -- @
    -- Because why not make `try { foo(); } catch () {}` an operator...
    deriving (Eq, Show)

-- | A binary operator, e.g. @+@ or @+=@.
newtype BinOp = MkBinOp { unBinOp :: Op }
    deriving (Eq, Show)

-- | A unary operator, e.g. @!@ or @~@.
newtype UnOp = MkUnOp { unUnOp :: Op }
    deriving (Eq, Show)

-- * Operator Tables

-- | The fixity of an operator.
data Fixity
    = InfixLeft
    | InfixRight
    | InfixNone
    | Prefix
    | Postfix
    deriving (Eq, Show)

type OperatorTable = [(Fixity, [(Op, String)])]

-- | For notational convenience.
(>:) = (,)

-- | For notational convenience.
(=:) = (,)

-- | All operators with higher precedence than @:?@ and assignment.
-- |
-- | These are broken up because we cannot parse both conditional expressions
-- | and assignments as usual operators.
higherOperators :: OperatorTable
higherOperators =
    [ InfixRight >:
        [ Exponentiate =: "**"
        ]
    , Prefix >:
        [ BitwiseNot =: "~"
        , Ignore =: "@"
        ]
    , InfixNone >:
        [ InstanceOf =: "instanceof"
        ]
    , Prefix >:
        [ Not =: "!"
        ]
    , InfixLeft >:
        [ Multiply =: "*"
        , Divide =: "/"
        , Modulo =: "%"
        ]
    , InfixLeft >:
        [ Add =: "+"
        , Subtract =: "-"
        , Concat =: "."
        ]
    -- NOTE: The reference doesn't specify the precedence of these.
    , Prefix >:
        [ Identity =: "+"
        , Negate =: "-"
        ]
    , InfixLeft >:
        [ ShiftLeft =: "<<"
        , ShiftRight =: ">>"
        ]
    , InfixNone >:
        [ LessThan =: "<"
        , GreaterThan =: ">"
        , LessThanEqual =: "<="
        , GreaterThanEqual =: ">="
        ]
    , InfixNone >:
        [ Equal =: "=="
        , Identical =: "==="
        , NotEqual =: "!="
        , NotEqualAlt =: "<>"
        , NotIdentical =: "!=="
        ]
    , InfixRight >:
        [ BitwiseAnd =: "&"
        ]
    , InfixLeft >:
        [ BitwiseOr =: "|"
        ]
    , InfixLeft >:
        [ BitwiseXor =: "^"
        ]
    , InfixLeft >:
        [ And =: "&&"
        ]
    , InfixLeft >:
        [ Or =: "||"
        ]
    ]

-- | All operators with lower precedence than @:?@ and assignment.
lowerOperators :: OperatorTable
lowerOperators =
    [ InfixLeft >:
        [ AndAlt =: "and"
        ]
    , InfixLeft >:
        [ Xor =: "xor"
        ]
    , InfixLeft >:
        [ OrAlt =: "or"
        ]
    ]

-- | Every operator, grouped by precedence and fixity.
-- |
-- | Based on http://php.net/manual/en/language.operators.precedence.php
operators :: OperatorTable
operators = higherOperators <> lowerOperators

data AssignOp
    = Assign -- =
    | AddAssign -- +=
    | SubtractAssign -- -=
    | MultiplyAssign -- *=
    | DivideAssign -- /=
    | ModuloAssign -- %=
    | ExpAssign -- **=
    | ConcatAssign -- .=
    | BWAndAssign -- &=
    | BWOrAssign -- |=
    | BWXorAssign -- ^=
    | LeftShiftAssign -- <<=
    | RightShiftAssign -- >>=
    deriving (Eq, Show)

assignOps :: [(AssignOp, String)]
assignOps =
    [ Assign =: "="
    , AddAssign =: "+="
    , SubtractAssign =: "-="
    , MultiplyAssign =: "*="
    , DivideAssign =: "/="
    , ModuloAssign =: "%="
    , ExpAssign =: "**="
    , ConcatAssign =: ".="
    , BWAndAssign =: "&="
    , BWOrAssign =: "|="
    , BWXorAssign =: "^="
    , LeftShiftAssign =: "<<="
    , RightShiftAssign =: ">>="
    ]
