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
    let base  = (() :. 4) :: (() :. Int)
        base2 = base :. (5 :: Int)
        base5 = base2 :. (6 :: Int)
                      :. (7 :: Int)
                      :. (8 :: Int)
        inc   = (fmap (+1)) :: ((s :. Int) :> (s :. Int))

    describe "stack type" $ do
        it "should take unit as base." $
            base `shouldBe` (() :. 4)
        it "should be a functor." $
            fmap (+2) base `shouldBe` (() :. 6)
        it "should be able to hold different types." $
            (base :. ("name" :: T.Text)) `shouldBe` (base :. "name")
        it "should have meaningful equality." $
            base `shouldNotBe` (() :. (5 :: Int))

    describe "stack manipulation functions" $ do
        describe "push" $
            it "should push something onto the top of the stack." $
                showSigil (push (2 :: Int)) `shouldBe` "2\n()\n"
        describe "pop" $
            it "should remove something from the top of the stack." $ do
                showSigil (pop :: SigilProgram Int ()) `shouldBe` ".\n()\n"
        describe "program" $
            it "should push and pop" $
                showSigil ((push 2 >> pop) :: SigilProgram Int ()) `shouldBe` "2\n.\n()\n"

    describe "control words" $ do
        let basef = base5 :. inc
            baser = base2 :. 6 :. 7 :. 9
        describe "apply" $
            it "should execute the function on top of the stack on the stack." $
                apply basef `shouldBe` baser
        describe "dip" $
            it "should apply under the top of the stack." $
                dip (basef :. 12) `shouldBe` (baser :. 12)
        describe "dip2" $
            it "should apply under the top two items on the stack." $
                dip2 (basef :. 12 :. 13) `shouldBe` (baser :. 12 :. 13)
        describe "dip3" $
            it "should apply under the top three items on the stack." $
                dip3 (basef :. 12 :. 13 :. 14) `shouldBe`
                    (baser :. 12 :. 13 :. 14)
        describe "dip4" $
            it "should apply under the top four items on the stack." $
                dip4 (basef :. 12 :. 13 :. 14 :. 15) `shouldBe`
                    (baser :. 12 :. 13 :. 14 :. 15)
        describe "keep" $
            it "should apply to the current stack, but keep the top." $
                keep (base :. 42 :. inc) `shouldBe` (base :. 43 :. 42)
        describe "keep2" $
            it "should apply to the current stack, but keep the top." $
                keep2 (base :. 42 :. 13 :. inc) `shouldBe`
                    (base :. 42 :. 14 :. 42 :. 13)
        describe "keep3" $
            it "should apply to the current stack, but keep the top." $
                keep3 (base :. 42 :. 13 :. 99 :. inc) `shouldBe`
                    (base :. 42 :. 13 :. 100 :. 42 :. 13 :. 99)


tests :: TestTree
tests = testGroup "sigil"
    [ testCase "language" specs
    ]

main :: IO ()
main = defaultMain tests

