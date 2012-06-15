{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Utils
    ( assertCode
    , assertText
    ) where

import           Control.Applicative
import qualified Data.Char as C
import           Data.Monoid (mempty)
import qualified Data.Text as T
-- import qualified Data.Vector.Unboxed as V
import           Language.Sigil
import           Test.HUnit (Assertion, assertBool)
import           Test.QuickCheck
import           Text.Printf

assertCode :: String -> SWord -> Quote -> Assertion
assertCode msg word expected = do
    actual <- fst `fmap` runSigilCode defaultEnv program
    let msg' = printf "%s: %s => %s" msg (show word) (show actual)
    assertBool msg' $ Stack expected == actual
    where
        program   = Program mempty [word]

assertText :: String -> Quote -> T.Text -> Assertion
assertText msg expected input = do
    case parseText input of
        Left err   -> fail $ printf "%s: invalid input: '%s'" msg input'
        Right prog -> do
            actual <- fst `fmap` runSigilCode defaultEnv prog
            let msg' = printf "%s: '%s' => %s" msg input' $ show actual
            assertBool msg' $ Stack expected == actual
    where
        input' = T.unpack input

-- Instance of Arbitrary for QC tests.

instance Arbitrary T.Text where
    arbitrary = fmap T.pack . listOf1 $ suchThat (choose chrRange) isNameChar
        where chrRange = ('*', 'z')

              isNameChar :: Char -> Bool
              isNameChar '.' = True
              isNameChar c   = C.isAlpha c

{-
 - instance (Arbitrary a, V.Unbox a) => Arbitrary (V.Vector a) where
 -     arbitrary = V.fromList <$> arbitrary
 -}

instance Arbitrary SWord where
    arbitrary = frequency [ (1, B  <$> arbitrary)
                          , (1, I  <$> arbitrary)
                          -- , (1, F  <$> arbitrary)
                          , (1, S  <$> arbitrary)
                          -- , (1, VI <$> arbitrary)
                          -- , (1, VF <$> arbitrary)
                          -- , (1, VB <$> arbitrary)
                          , (2, Q  <$> resize 3 (listOf arbitrary))
                          ]

instance Arbitrary Stack where
    arbitrary = Stack <$> resize 3 (listOf arbitrary)


