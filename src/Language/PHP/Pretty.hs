{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.PHP.Pretty where

import Data.Char (toLower)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.Semigroup
import qualified Data.Text as T

import Data.Text.Prettyprint.Doc hiding (Doc)
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.Text

import Language.PHP.AST
import Language.PHP.AST.Ops

type Doc = forall ann. PP.Doc ann

toString :: Pretty a => a -> String
toString = T.unpack . renderStrict . layoutCompact . pretty

instance Pretty Op where
    pretty = pretty . opSymbol

instance Pretty UnOp where
    pretty (MkUnOp op) = pretty op

instance Pretty BinOp where
    pretty (MkBinOp op) = pretty op

instance Pretty AssignOp where
    pretty op = pretty $ fromJust $ lookup op assignOps

instance Pretty Literal where
    pretty (Int i) = pretty i
    pretty (Float f) = pretty f
    pretty (Bool b) = pretty $ map toLower $ show b
    pretty (SingleQuotes s) = squotes $ pretty s
    pretty (DoubleQuotes s) = dquotes $ pretty s
    pretty (Backticks s) = "`" <> pretty s <> "`"
    pretty (Null) = "NULL"

instance Pretty Var where
    pretty (SimpleVar i) = "$" <> pretty i
    pretty (VarVar v') = "$" <> pretty v'
    pretty (ExprVar e) = "${" <+> pretty e <+> "}"

instance Pretty Assignment where
    pretty = \case
        ByRef lhs rhs -> hsep [pretty lhs, "=", "&" <> pretty rhs]
        ByValue op lhs rhs ->
            let
                -- We (ab)use the 'Foldable' instance of 'Maybe' here, so that
                -- @Nothing@ maps to @False@, and @Just b@ to @b@.
                lower = or $ do
                    op <- operatorOf rhs
                    pure $ any (\(op', _, _) -> op == op') $ concat lowerOperators
            in
                hsep [pretty lhs, pretty op, parenthesize lower $ pretty rhs]

-- * Prettifying Expressions

-- | Parts of this section are based on Norman Ramsey, "Unparsing Expressions
-- With Prefix and Postfix Operators"
-- (https://pdfs.semanticscholar.org/6a07/5485596524c5b69e4a13165d5bb911498bf5.pdf)

-- | Parenthesize a 'Doc' only if a condition holds.
parenthesize :: Bool -> Doc -> Doc
parenthesize cond doc = if cond then parens doc else doc

-- | Check whether a nesting of operators needs parentheses to disambiguate.
-- |
-- | The first operator is the 'inner' operator, e.g. the @+@ in the expression
-- | @(1 + 2) * 3@, and the second is the 'outer' one, e.g. @*@.
-- |
-- | The associativity indicates whether we are recursing into the left hand
-- | side or the right hand side of a binary operator, or that the operator is
-- | unary.
needsParens :: Op -> Op -> Associativity -> Bool
-- It is easier to check whether parens would be redundant, so we check
-- the negation.
needsParens inner outer side = not $
    -- TODO: This is ugly stuff.
    if inner `elem` binOps && outer `elem` binOps && precedence inner > precedence outer
    -- If the inner operator has greater precedence than the outer
    -- operator, we definitely don't need parens.
    then True
    -- Otherwise, it depends on the fixity of the inner operator, and
    -- on what side of the outer operator we are.
    else case (fixity inner, side) of
        -- Chaining equally precedent operators with the same
        -- associativity does not need parens.
        -- E.g. `1 + 2 - 3` = `(1 + 2) - 3`.
        (InfixLeft, AssocLeft) ->
            precedence inner == precedence outer
            && fixity outer == InfixLeft
        -- E.g. `1 & 2 & 3` = `1 & (2 & 3)`.
        (InfixRight, AssocRight) ->
            precedence inner == precedence outer
            && fixity outer == InfixRight

        -- We need parens in any other case.
        _ -> False

        -- Note that we do not do anything with unary operators. This is because
        -- Megaparsec currently has some issues with parsing them in some cases.
        -- So we always parenthesize them.
        -- See: https://github.com/mrkkrp/megaparsec/issues/132

prettyExpr :: Expr -> Doc
prettyExpr (Literal l) = pretty l
prettyExpr (Var v) = pretty v
prettyExpr (Const i) = pretty i
prettyExpr (BinOp (MkBinOp op) lhs rhs) =
    hsep [prettyPrec op AssocLeft lhs, pretty op, prettyPrec op AssocRight rhs]
prettyExpr (UnOp (MkUnOp op) expr) =
    hcat [pretty op, prettyPrec op AssocNone expr]
prettyExpr (IncDec fixity delta var) = parens $
    let
        op = case delta of
            Increment -> "++"
            Decrement -> "--"
    in
        case fixity of
            Prefix -> op <> pretty var
            Postfix -> pretty var <> op
prettyExpr (Assignment ass) = parens $ pretty ass
prettyExpr (Conditional c t f) =
    parens $ hsep [subExpr c, "?", fold $ subExpr <$> t, ":", subExpr f]
    where
    subExpr expr =
        let
            -- We (ab)use the 'Foldable' instance of 'Maybe' here, so that
            -- @Nothing@ maps to @False@, and @Just b@ to @b@.
            lower = or $ do
                op <- operatorOf expr
                pure $ any (\(op', _, _) -> op == op') $ concat lowerOperators
        in
            parenthesize lower $ prettyExpr expr

prettyExpr _ = error "unimplemented"

prettyPrec :: Op -> Associativity -> Expr -> Doc
prettyPrec prev assoc = \case
    BinOp (MkBinOp op) lhs rhs ->
        parenthesize (needsParens op prev assoc) $
            hsep [prettyPrec op AssocLeft lhs, pretty op, prettyPrec op AssocRight rhs]
    UnOp (MkUnOp op) expr ->
        parenthesize (needsParens op prev assoc) $
            -- Note: we always prettify as a prefix operator, because
            -- there are no 'plain' postfix unops; `++` and `--` only
            -- work on variables, and are a different type.
            hcat [pretty op, prettyPrec op AssocNone expr]
    expr -> prettyExpr expr

instance Pretty Expr where
    pretty = prettyExpr
