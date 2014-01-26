{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Main where


import qualified Data.Text        as T
import           Test.Tasty
import           Test.Tasty.Hspec
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.HUnit as HU
-- import Test.Tasty.Golden as TG

import           Language.Sigil


shouldNotBe :: (Eq a, Show a) => a -> a -> Expectation
shouldNotBe a b = shouldSatisfy a (/= b)

specs :: Spec
specs = do
    let base = (() :. 4) :: (() :. Int)
    describe "stack type" $ do
        it "should take unit as base." $
            base `shouldBe` (() :. 4)
        it "should be a functor." $
            fmap (+2) base `shouldBe` (() :. 6)
        it "should be able to hold different types." $
            (base :. ("name" :: T.Text)) `shouldBe`
                (base :. ("name" :: T.Text))
        it "should have meaningful equality." $
            base `shouldNotBe` (() :. (5 :: Int))

    describe "stack functions" $
        describe "swap" $
            it "should swap the top two items on the stack." $
                swap (base :. (2 :: Int)) `shouldBe` (() :. 2 :. 4)

tests :: TestTree
tests = testGroup "main"
    [ testCase "something" specs
    ]

main :: IO ()
main = defaultMain tests

