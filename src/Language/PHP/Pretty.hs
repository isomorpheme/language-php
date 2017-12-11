{-# LANGUAGE OverloadedStrings #-}

module Language.PHP.Pretty where

import Data.Char (toLower)
import Data.Semigroup

import Text.PrettyPrint (Doc, (<+>))
import qualified Text.PrettyPrint as PP

import Language.PHP.AST

class Pretty p where
    pretty :: p -> PP.Doc

instance Pretty UnOp where
    pretty op =
        case op of
            Identity -> "+"
            Negate -> "-"
            Not -> "!"

instance Pretty BinOp where
    -- TODO: just use a lookup table for this so that parsing is easier too
    pretty op =
        case op of
            Add -> "+"
            Subtract -> "-"
            Multiply -> "*"
            Divide -> "/"
            Modulo -> "%"
            Exponentiate -> "**"
            _ -> error "unimplemented"

instance Pretty Literal where
    pretty (Int i) = PP.int i
    pretty (Float f) = PP.float f
    pretty (Bool b) = PP.text $ map toLower $ show b
    pretty (SingleQuotes s) = PP.quotes $ PP.text s
    pretty (DoubleQuotes s) = PP.doubleQuotes $ PP.text s
    pretty (Backticks s) = "`" <> PP.text s <> "`"
    pretty (Null) = "NULL"

instance Pretty Var where
    pretty (SimpleVar i) = "$" <> PP.text i
    pretty (VarVar v') = "$" <> pretty v'
    pretty (ExprVar e) = "${" <+> pretty e <+> "}"

instance Pretty Expr where
    pretty (BinOp op lhs rhs) =
        PP.parens (pretty lhs <+> pretty op <+> pretty rhs)
    pretty (UnOp op e) =
        pretty op <> PP.parens (pretty e)
    pretty (Literal l) = pretty l
    pretty (Var v) = pretty v
    pretty (Const i) = PP.text i
    pretty (Conditional c t f) =
        pretty c <+> "?" <+> pretty t <+> ":" <+> pretty f
    pretty _ = error "unimplemented"
