module Language.PHP.AST.Ops where

import Data.List (union)

-- | The fixity of an operator.
data Fixity
    = InfixLeft
    | InfixRight
    | InfixNone
    | Prefix
    | Postfix

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

    --- Unary Operators
    -- Arithmetic
    | Identity -- +
    | Negate -- -

    -- Impure Arithmetic
    | PreIncrement -- ++
    | PreDecrement -- --
    | PostIncrement -- ++
    | PostDecrement -- --

    -- Logical
    | Not -- !

    -- Bitwise
    | BitwiseNot -- ~

    -- Error Control
    | Ignore -- @
    -- Because why not make `try { foo(); } catch () {}` an operator...
    deriving (Eq, Show)

-- | A binary operation, e.g. @+@ or @+=@.
newtype BinOp = MkBinOp { unBinOp :: Op }
    deriving (Eq, Show)

-- | A unary operation, e.g. @!@ or @~@.
newtype UnOp = MkUnOp { unUnOp :: Op }
    deriving (Eq, Show)

-- | Every operator, grouped by precedence and fixity.
-- |
-- | Based on http://php.net/manual/en/language.operators.precedence.php
operators :: [(Fixity, [(Op, String)])]
operators =
    [ InfixRight >:
        [ Exponentiate =: "**"
        ]
    , Prefix >:
        [ PreIncrement =: "++"
        , PreDecrement =: "--"
        , BitwiseNot =: "~"
        , Ignore =: "@"
        ]
    -- NOTE: The reference doesn't explicitly list the precedence of these.
    , Postfix >:
        [ PostIncrement =: "++"
        , PostDecrement =: "--"
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
    , InfixRight >:
        [ Assign =: "="
        , AddAssign =: "+="
        , SubtractAssign =: "-="
        , MultiplyAssign =: "*="
        , DivideAssign =: "/="
        , ConcatAssign =: ".="
        , ModuloAssign =: "%="
        , BWAndAssign =: "&="
        , BWOrAssign =: "|="
        , BWXorAssign =: "^="
        , LeftShiftAssign =: "<<="
        , RightShiftAssign =: ">>="
        ]
    , InfixLeft >:
        [ AndAlt =: "and"
        ]
    , InfixLeft >:
        [ Xor =: "xor"
        ]
    , InfixLeft >:
        [ OrAlt =: "or"
        ]
    ]
    where
    -- For notational convenience
    (>:) = (,)
    (=:) = (,)
