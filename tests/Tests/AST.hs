module Tests.AST where

import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.PHP.AST
import Language.PHP.AST.Ops

-- | Generate an identifier that looks like a variable or member name, i.e.
-- | starts with a lowercase character.
genVarIdent :: MonadGen m => m Ident
genVarIdent = (:) <$> Gen.lower <*> Gen.string (Range.singleton 2) Gen.alphaNum

-- | Generate an identifier that looks more like a constant, i.e. consists only
-- | of uppercase characters.
genConstIdent :: MonadGen m => m Ident
genConstIdent = Gen.string (Range.linear 4 10) Gen.upper

genLiteral :: MonadGen m => m Literal
genLiteral = Gen.choice
    [ Int <$> Gen.word Range.exponentialBounded
    , Float <$> Gen.float (Range.exponentialFloat 0 1024)
    , Bool <$> Gen.bool
    , SingleQuotes <$> genString
    , DoubleQuotes <$> genString
    , Backticks <$> genString
    , pure Null
    ]
    where
    genString = Gen.string (Range.linear 0 50) Gen.unicode

genVar :: MonadGen m => m Var
genVar = Gen.recursive Gen.choice
    [ SimpleVar <$> genVarIdent
    , ExprVar <$> genExpr
    ]
    [ Gen.subterm genVar VarVar
    ]

genVarExpr :: MonadGen m => m Expr
genVarExpr = Gen.shrink shrinker $ Var <$> genVar
    where
    shrinker (Var (ExprVar expr)) = [expr]
    shrinker _ = []

genAssignOp :: MonadGen m => m AssignOp
genAssignOp = Gen.enumBounded

genAssignment :: MonadGen m => m Assignment
genAssignment = Gen.choice
    [ ByValue <$> genAssignOp <*> genVar <*> genExpr
    , ByRef <$> genVar <*> genVar
    ]

genAssignmentExpr :: MonadGen m => m Expr
genAssignmentExpr = Gen.shrink shrinker $ Assignment <$> genAssignment
    where
    shrinker (Assignment a) = case a of
        ByValue _op var expr -> [Var var, expr]
        ByRef var ref -> [Var var, Var ref]
    shrinker _ = []

genBinOp :: MonadGen m => m BinOp
genBinOp = MkBinOp <$> Gen.element binOps

genUnOp :: MonadGen m => m UnOp
genUnOp = MkUnOp <$> Gen.element unOps

genFixity :: MonadGen m => m Fixity
genFixity = Gen.enumBounded

-- | Generate one of just @[Prefix, Postfix]@
genFixity' :: MonadGen m => m Fixity
genFixity' = Gen.element [Prefix, Postfix]

genDelta :: MonadGen m => m Delta
genDelta = Gen.enumBounded

genIncDecExpr :: MonadGen m => m Expr
genIncDecExpr = Gen.shrink shrinker $ IncDec <$> genFixity' <*> genDelta <*> genVar
    where
    shrinker (IncDec _fx _d var) = [Var var]
    shrinker _ = []

genExpr :: MonadGen m => m Expr
genExpr = Gen.recursive Gen.choice
    [ Const <$> genConstIdent
    , Literal <$> genLiteral
    , genAssignmentExpr
    , genVarExpr
    , genIncDecExpr
    ]
    [ Gen.subtermM2 genExpr genExpr $
        \lhs rhs -> BinOp <$> genBinOp <*> pure lhs <*> pure rhs
    , Gen.subtermM genExpr $
        \e -> UnOp <$> genUnOp <*> pure e
    , Gen.subtermM2 genExpr genExpr $
        \lhs rhs -> Conditional <$> pure lhs <*> Gen.maybe genExpr <*> pure rhs
    ]
