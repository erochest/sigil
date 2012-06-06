{-# LANGUAGE OverloadedStrings #-}

module Test.Sigil.Types
    ( typeTests
    ) where

import           Control.Applicative
import qualified Data.Char as C
import           Data.Monoid
import qualified Data.Text as T
import           Language.Sigil
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

-- Instance of Arbitrary for QC tests.

instance Arbitrary T.Text where
    arbitrary = fmap T.pack . listOf1 $ suchThat (choose chrRange) isNameChar
        where chrRange = ('*', 'z')

              isNameChar :: Char -> Bool
              isNameChar '.' = True
              isNameChar c   = C.isAlpha c

instance Arbitrary SWord where
    arbitrary = frequency [ (1, B <$> arbitrary)
                          , (1, I <$> arbitrary)
                          , (1, F <$> arbitrary)
                          , (1, S <$> arbitrary)
                          , (2, Q <$> resize 3 (listOf arbitrary))
                          ]

instance Arbitrary Stack where
    arbitrary = Stack <$> resize 3 (listOf arbitrary)

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

pShowF :: Double -> Bool
pShowF f = show f == show (F f)

pShowS :: T.Text -> Bool
pShowS s = T.unpack s == show (S s)

testShowQ :: Assertion
testShowQ = do
    ab $ show (Q []) == "[ ]"
    ab $ show (Q [I 4]) == "[ 4 ]"
    ab $ show (Q [I 5, S "+"]) == "[ 5 + ]"
    ab $ show (Q [I 5, F 3.1415, S "+"]) == "[ 5 3.1415 + ]"
    ab $ show (Q [I 5, Q [S "+", I 13]]) == "[ 5 [ + 13 ] ]"
    where ab = assertBool "testShowQ"

pWordEq :: SWord -> Bool
pWordEq a = a == a

-- Stack tests.



-- Tests.

typeTests :: [Test]
typeTests =
    [ testGroup "word"  [ testProperty "word-mempty" (pMEmpty :: SWord -> Bool)
                        , testProperty "word-mappend" (pMAppend :: (SWord, SWord, SWord) -> Bool)
                        , testProperty "word-mconcat" (pMConcat :: [SWord] -> Bool)
                        , testCase "show-b" testShowB
                        , testProperty "show-i" pShowI
                        , testProperty "show-f" pShowF
                        , testProperty "show-s" pShowS
                        , testCase "show-q" testShowQ
                        , testProperty "word-eq" pWordEq
                        ]
    , testGroup "stack" [ testProperty "stack-mempty" (pMEmpty :: Stack -> Bool)
                        , testProperty "stack-mappend" (pMAppend :: (Stack, Stack, Stack) -> Bool)
                        , testProperty "stack-mconcat" (pMConcat :: [Stack] -> Bool)
                        ]
    ]

