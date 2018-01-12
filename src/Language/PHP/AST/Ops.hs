module Language.PHP.AST.Ops where

import Control.Monad (guard)
import Data.Char (isAlpha)
import Data.Foldable (foldMap)
import Data.List (find, union)
import Data.Maybe (fromJust)
import Data.Monoid (First(..))
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

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
    deriving (Bounded, Enum, Eq, Show)

-- TODO: We can just determine these from 'operators'
binOps :: [Op]
binOps =
    [ Add
    , Subtract
    , Multiply
    , Divide
    , Modulo
    , Exponentiate
    , And
    , Or
    , Xor
    , AndAlt
    , OrAlt
    -- , Coalesce -- PHP 7 only
    , Equal
    , Identical
    , NotEqual
    , NotEqualAlt
    , NotIdentical
    , LessThan
    , GreaterThan
    , LessThanEqual
    , GreaterThanEqual
    -- , Spaceship
    , BitwiseAnd
    , BitwiseOr
    , BitwiseXor
    , ShiftLeft
    , ShiftRight
    , Concat
    , InstanceOf
    ]

unOps :: [Op]
unOps =
    [ Identity
    , Negate
    , Not
    , BitwiseNot
    , Ignore
    ]

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
    deriving (Bounded, Enum, Eq, Show)

-- | The associativity of an operator.
data Associativity
    = AssocLeft
    | AssocRight
    | AssocNone

associativity :: Fixity -> Associativity
associativity InfixLeft = AssocLeft
associativity InfixRight = AssocRight
associativity InfixNone = AssocNone
associativity Prefix = AssocNone
associativity Postfix = AssocNone

type OpDescription = (Op, Fixity, String)
type OperatorTable = [[OpDescription]]

-- | For notational convenience.
(>:) fx = fmap $ \(op, sym) -> (op, fx, sym)

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
        [ Identity =: "+"
        , Negate =: "-"
        , BitwiseNot =: "~"
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

-- | Every character which appears in an operator at least once.
operatorChars :: Set Char
operatorChars = Set.union opChars assignChars
    where
    opChars = Set.fromList $ do
        (_, _, sym) <- concat $ operators
        guard $ not $ any isAlpha sym
        sym
    assignChars = Set.fromList $ assignOps >>= snd

-- | Get the description of an operator, i.e. its value, fixity and literal symbol.
description :: Op -> OpDescription
-- 'fromJust' can be used here, since every operator *should* have a defined
-- fixity and symbol.
description op = fromJust $ find (\(op', _, _) -> op == op') $ concat operators

-- | Get the fixity of an operator.
fixity :: Op -> Fixity
fixity op = case description op of (_op, fx, _sym) -> fx

-- | Get the symbol (i.e. its literal string) of an operator.
opSymbol :: Op -> String
opSymbol op = case description op of (_op, _fx, sym) -> sym

-- | Get a number representing the precedence of an operator.
precedence :: Op -> Int
precedence op = (maxPrec -)
    -- Same justification for 'fromJust' as before.
    $ fromJust
    $ findMap (check op)
    $ concat
    $ zipWith (fmap . (,)) [0..] operators
    where
    maxPrec = length operators - 1
    check op (prec, (op', _, _)) = prec <$ guard (op' == op)
    findMap f = getFirst . foldMap (First . f)

-- TODO: Maybe add all constructors to `Op` and add a newtype.
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
    deriving (Bounded, Enum, Eq, Show)

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
