import Data.Functor (void)

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog as HH

import Language.PHP.Pretty
import Language.PHP.Parser

import Tests.AST

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ HH.testProperty "expressions prettify without errors" $ property $ do
        expr <- forAll genExpr
        void $ eval $ toString expr
    , HH.testProperty "parsing/printing expressions roundtrips" $ property $ do
        expr <- forAll genExpr
        tripping expr toString parsePretty
    ]
