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

instance Arbitrary T.Text where
    arbitrary = fmap T.pack . listOf1 $ suchThat (choose chrRange) isNameChar
        where chrRange = ('*', 'z')

              isNameChar :: Char -> Bool
              isNameChar '.' = True
              isNameChar c   = C.isAlpha c

instance Arbitrary SWord where
    arbitrary = frequency [ (1, B <$> arbitrary)
                          , (1, I <$> arbitrary)
                          , (1, S <$> arbitrary)
                          , (2, Q <$> resize 5 (listOf arbitrary))
                          ]

--

pWordMEmpty :: SWord -> Bool
pWordMEmpty code = (mempty `mappend` code) == code &&
                   (code `mappend` mempty) == code

pWordMAppend :: (SWord, SWord, SWord) -> Bool
pWordMAppend (x, y, z) = mappend x (mappend y z) == mappend (mappend x y) z

testShowB :: Assertion
testShowB = do
    assertBool "testShowB" $ show (B True)  == "#t"
    assertBool "testShowB" $ show (B False) == "#f"

pShowI :: Int -> Bool
pShowI i = show i == show (I i)

pShowS :: T.Text -> Bool
pShowS s = T.unpack s == show (S s)

testShowQ :: Assertion
testShowQ = do
    ab $ show (Q []) == "[ ]"
    ab $ show (Q [I 4]) == "[ 4 ]"
    ab $ show (Q [I 5, S "+"]) == "[ 5 + ]"
    ab $ show (Q [I 5, Q [S "+", I 13]]) == "[ 5 [ + 13 ] ]"
    where ab = assertBool "testShowQ"

pWordEq :: SWord -> Bool
pWordEq a = a == a

--

typeTests :: [Test]
typeTests =
    [ testGroup "word"  [ testProperty "word-mempty" pWordMEmpty
                        , testProperty "word-mappend" pWordMAppend
                        , testCase "show-b" testShowB
                        , testProperty "show-i" pShowI
                        , testProperty "show-s" pShowS
                        , testCase "show-q" testShowQ
                        , testProperty "word-eq" pWordEq
                        ]
    , testGroup "stack" [
                        ]
    ]

