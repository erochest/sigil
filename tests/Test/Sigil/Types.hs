{-# LANGUAGE OverloadedStrings #-}

module Test.Sigil.Types
    ( typeTests
    ) where

import           Data.Monoid
import qualified Data.Text as T
-- import qualified Data.Vector.Unboxed as V
import           Language.Sigil
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck
import           Test.Utils ()

-- Generic QC properties for Monoids.

pMEmpty :: (Arbitrary a, Monoid a, Eq a) => a -> Bool
pMEmpty input = (mempty `mappend` input) == input &&
                (input `mappend` mempty) == input

pMAppend :: (Arbitrary a, Monoid a, Eq a) => (a, a, a) -> Bool
pMAppend (x, y, z) = mappend x (mappend y z) == mappend (mappend x y) z

pMConcat :: (Arbitrary a, Monoid a, Eq a) => [a] -> Bool
pMConcat input = mconcat input == foldr mappend mempty input

-- SWord tests.

testShowB :: Assertion
testShowB = do
    assertBool "testShowB" $ show (B True)  == "#t"
    assertBool "testShowB" $ show (B False) == "#f"

pShowI :: Int -> Bool
pShowI i = show i == show (I i)

{-
 - pShowF :: Double -> Bool
 - pShowF f = show f == show (F f)
 -}

pShowS :: T.Text -> Bool
pShowS s = T.unpack s == show (S s)

testShowQ :: Assertion
testShowQ = do
    ab $ show (Q []) == "[ ]"
    ab $ show (Q [I 4]) == "[ 4 ]"
    ab $ show (Q [I 5, S "+"]) == "[ 5 + ]"
    -- ab $ show (Q [I 5, F 3.1415, S "+"]) == "[ 5 3.1415 + ]"
    ab $ show (Q [I 5, I 3, S "+"]) == "[ 5 3 + ]"
    ab $ show (Q [I 5, Q [S "+", I 13]]) == "[ 5 [ + 13 ] ]"
    where ab = assertBool "testShowQ"

{-
 - testShowVI :: Assertion
 - testShowVI = do
 -     avi $ svi []                     == "#i< >"
 -     avi $ svi [1, 2, 3]              == "#i< 1 2 3 >"
 -     avi $ svi [13, 42, 74, 100, 121] == "#i< 13 42 74 100 121 >"
 -     where avi = assertBool "testShowVI"
 -           svi = show . VI . V.fromList
 - 
 - testShowVF :: Assertion
 - testShowVF = do
 -     avf $ svf []                        == "#f< >"
 -     avf $ svf [-1.0, 0.0, 1.0]          == "#f< -1.0 0.0 1.0 >"
 -     avf $ svf [1.4142, 2.71828, 3.1415] == "#f< 1.4142 2.71828 3.1415 >"
 -     where avf = assertBool "testShowVF"
 -           svf = show . VF . V.fromList
 - 
 - testShowVB :: Assertion
 - testShowVB = do
 -     avb (svb [])                               "#b<>"
 -     avb (svb [True, False, True])              "#b<101>"
 -     avb (svb [True, True, False, False, True]) "#b<11001>"
 -     where avb i j = assertBool ("testShowVB: " ++ show i) $ i == j
 -           svb = show . VB . V.fromList
 -}

pWordEq :: SWord -> Bool
pWordEq a = a == a

-- Stack tests.



-- Tests.

typeTests :: [Test]
typeTests =
    [ testGroup "word"  [ testProperty "word-mempty"  (pMEmpty :: SWord -> Bool)
                        , testProperty "word-mappend" (pMAppend :: (SWord, SWord, SWord) -> Bool)
                        , testProperty "word-mconcat" (pMConcat :: [SWord] -> Bool)
                        , testCase "show-b" testShowB
                        , testProperty "show-i" pShowI
                        -- , testProperty "show-f" pShowF
                        , testProperty "show-s" pShowS
                        , testCase "show-q" testShowQ
                        -- , testCase "show-vi" testShowVI
                        -- , testCase "show-vf" testShowVF
                        -- , testCase "show-vb" testShowVB
                        , testProperty "word-eq" pWordEq
                        ]
    , testGroup "stack" [ testProperty "stack-mempty"  (pMEmpty :: Stack -> Bool)
                        , testProperty "stack-mappend" (pMAppend :: (Stack, Stack, Stack) -> Bool)
                        , testProperty "stack-mconcat" (pMConcat :: [Stack] -> Bool)
                        ]
    ]

