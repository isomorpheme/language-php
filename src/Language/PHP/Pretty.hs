{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PHP.Pretty where

import Data.Char (toLower)
import Data.Maybe (fromJust)
import Data.Semigroup
import qualified Data.Text as T

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Language.PHP.AST
import Language.PHP.AST.Ops

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
    pretty = parens . \case
        ByRef lhs rhs -> pretty lhs <+> "=" <+> ("&" <> pretty rhs)
        ByValue op lhs rhs -> pretty lhs <+> pretty op <+> pretty rhs

instance Pretty Expr where
    pretty (BinOp op lhs rhs) =
        parens (pretty lhs <+> pretty op <+> pretty rhs)
    pretty (UnOp op e) =
        pretty op <> parens (pretty e)
    pretty (Literal l) = pretty l
    pretty (Var v) = pretty v
    pretty (Const i) = pretty i
    pretty (IncDec fixity delta var) =
        let
            op = case delta of
                Increment -> "++"
                Decrement -> "--"
        in
            case fixity of
                Prefix -> op <> pretty var
                Postfix -> pretty var <> op
    pretty (Assignment ass) = pretty ass
    pretty (Conditional c t f) =
        parens $ pretty c <+> "?" <+> pretty t <+> ":" <+> pretty f
    pretty _ = error "unimplemented"
