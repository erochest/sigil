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

    describe "stack type" $ do
        it "should take unit as base." $
            base `shouldBe` (() :. 4)
        it "should be a functor." $
            fmap (+2) base `shouldBe` (() :. 6)
        it "should be able to hold different types." $
            (base :. ("name" :: T.Text)) `shouldBe` (base :. "name")
        it "should have meaningful equality." $
            base `shouldNotBe` (() :. (5 :: Int))

    describe "stack functions" $ do
        describe "swap" $
            it "should swap the top two items on the stack." $
                swap (base :. (2 :: Int)) `shouldBe` (() :. 2 :. 4)
        describe "push" $
            it "should push something onto the top of the stack." $
                push (2 :: Int) base `shouldBe` (base :. 2)
        describe "pop" $
            it "should remove something from the top of the stack." $ do
                pop (base :. (2 :: Int)) `shouldBe` base
                pop base `shouldBe` ()
        describe "nip" $
            it "should remove the second item from the stack." $
                nip base5 `shouldBe` (base2 :. 6 :. 8)
        describe "nip2" $
            it "should remove the second and third items from the stack." $
                nip2 base5 `shouldBe` (base2 :. 8)
        describe "rotr" $
            it "should rotate the top 3 items right." $
                rotr base5 `shouldBe` (base2 :. 8 :. 6 :. 7)
        describe "rotl" $
            it "should rotate the top 3 items left." $
                rotl base5 `shouldBe` (base2 :. 7 :. 8 :. 6)

-- swapd :: s :. a :. b :. c -> s :. b :. a :. c
-- dup   :: s :. a -> s :. a :. a
-- dup2  :: s :. a :. b -> s :. a :. b :. a :. b
-- dupd  :: s :. a :. b -> s :. a :. a :. b
-- over  :: s :. a :. b -> s :. a :. b :. a
-- over2 :: s :. a :. b :. c -> s :. a :. b :. c :. a :. b
-- pick  :: s :. a :. b :. c -> s :. a :. b :. c :. a

tests :: TestTree
tests = testGroup "sigil"
    [ testCase "language" specs
    ]

main :: IO ()
main = defaultMain tests

