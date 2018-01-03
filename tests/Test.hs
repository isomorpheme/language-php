import Data.Functor (void)

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog as HH

import Language.PHP.Pretty

import Tests.AST

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ HH.testProperty "expressions prettify without errors" $ property $ do
        expr <- forAll genExpr
        void $ eval $ pretty expr
    ]
