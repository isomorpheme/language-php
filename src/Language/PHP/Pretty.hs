{-# LANGUAGE OverloadedStrings #-}

module Language.PHP.Pretty where

import Data.Char (toLower)
import Data.Maybe (fromJust)
import Data.Semigroup

import Text.PrettyPrint (Doc, (<+>))
import qualified Text.PrettyPrint as PP

import Language.PHP.AST
import Language.PHP.AST.Ops

class Pretty p where
    pretty :: p -> PP.Doc

instance Pretty Op where
    pretty op = PP.text
        -- We can use 'fromJust' here, because there should be an entry for every
        -- operator. If not, we've forgotten to fully define an operator!
        $ fromJust
        $ lookup op $ concat $ fmap snd $ operators

instance Pretty UnOp where
    pretty (MkUnOp op) = pretty op

instance Pretty BinOp where
    pretty (MkBinOp op) = pretty op

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
