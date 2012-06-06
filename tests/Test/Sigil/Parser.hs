{-# LANGUAGE OverloadedStrings #-}

module Test.Sigil.Parser
    ( parserTests
    ) where

import qualified Data.Text as T
import           Language.Sigil
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Text.Printf

-- Utilities

{-
 - assertLeft :: String -> T.Text -> Assertion
 - assertLeft msg input =
 -     assertBool msg' $ either (const True) (const False) actual
 -     where msg'   = printf "%s %s => %s" msg (show input) (show actual)
 -           actual = parseText input
 -}

assertParse :: String -> SWord -> T.Text -> Assertion
assertParse msg expected input =
    assertBool msg' $ (Right expected) == actual
    where msg'   = printf "%s %s => %s /= %s"
                          msg (show input) (show expected) (show actual)
          actual = parseText input

-- Tests

assertLiteralBool :: Assertion
assertLiteralBool = do
    a True  "#t"
    a False "#f"
    a True  "#T"
    a False "#F"
    where a :: Bool -> T.Text -> Assertion
          a = assertParse "assertLiteralBool" . B

assertLiteralFloat :: Assertion
assertLiteralFloat = do
    a 2.718281 "2.718281"
    a 3.141592 "3.141592"
    a 0.33     "0.33"
    a (-0.33)  "-0.33"
    where
        a :: Double -> T.Text -> Assertion
        a = assertParse "assertLiteralFloat" . F

assertLiteralInt :: Assertion
assertLiteralInt = do
    a 13   "13"
    a 42   "42"
    a 113  "113"
    a (-3) "-3"
    a 0    "0"
    a 0    "-0"
    where a :: Int -> T.Text -> Assertion
          a = assertParse "assertLiteralInt" . I

assertInstructionAlpha :: Assertion
assertInstructionAlpha = do
    a "aaa" "aaa"
    a "bbb" "BBB"
    a "ccc" "CcC"
    a "abcdefghijklmnopqrstuvwxyz" "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    where
        a :: T.Text -> T.Text -> Assertion
        a = assertParse "assertInstructionAlpha" . S

assertInstructionAlphaNum :: Assertion
assertInstructionAlphaNum = do
    a "a888" "A888"
    a "g9h0i1" "G9h0I1"
    where
        a :: T.Text -> T.Text -> Assertion
        a = assertParse "assertInstructionAlphaNum" . S

assertInstructionSymbol :: Assertion
assertInstructionSymbol = do
    a "+" "+"
    a "*" "*"
    a "%" "%"
    a "-" "-"
    a "/" "/"
    a "<" "<"
    a "=" "="
    a ">" ">"
    where
        a :: T.Text -> T.Text -> Assertion
        a = assertParse "assertInstructionSymbol" . S

assertProgramEmpty :: Assertion
assertProgramEmpty = do
    a "[]"
    a "[ ]"
    a "[\n]"
    where a = assertParse "assertProgramEmpty" (Q [])

assertProgramSingle :: Assertion
assertProgramSingle = do
    a (Q [B True])        "[#t]"
    a (Q [F 3.14])        "[3.14]"
    a (Q [I 42])          "[42]"
    a (Q [S "+"]) "[+]"
    where a = assertParse "assertProgramSingle"

assertProgramExtraWS :: Assertion
assertProgramExtraWS = do
    a (Q [B True])        "[ #t ]"
    a (Q [F 3.14])        "[ 3.14 ]"
    a (Q [I 42])          "[ 42 ]"
    a (Q [S "+"]) "[ + ]"
    where a = assertParse "assertProgramExtraWS"

assertProgramEmbedded :: Assertion
assertProgramEmbedded = do
    a (Q [Q [B True]])         "[ [#t] ]"
    a (Q [Q [I 42  ]])         "[ [ 42 ] ]"
    a (Q [Q [Q [F 3.141592]]]) "[[[3.141592]] ]"
    where a = assertParse "assertProgramEmbedded"

assertProgramMultiple :: Assertion
assertProgramMultiple = do
    a (Q [I 42, I 13]) "[ 42 13 ]"
    a (Q [I 42, I 13, F 3.141592]) "[ 42 13 3.141592 ]"
    a (Q [I 42, Q [I 13], S "+"]) "[42 [13] +]"
    a (Q [I 42, Q [I 13, F 3.141592], S "+"]) "[42 [13 3.141592] +]"
    a (Q [Q [S "a", S "b"], Q [S "d", S "e"], Q [S "f", S "g"]])
      "[ [a b] [ d e ] [f g] ]"
    where a = assertParse "assertProgramMultiple"

--

parserTests :: [Test]
parserTests =
    [ testGroup "literals"      [ testCase "boolean" assertLiteralBool
                                , testCase "float" assertLiteralFloat
                                , testCase "integer" assertLiteralInt
                                ]
    , testGroup "instructions"  [ testCase "alpha" assertInstructionAlpha
                                , testCase "alphanum" assertInstructionAlphaNum
                                , testCase "symbol" assertInstructionSymbol
                                ]
    , testGroup "programs"      [ testCase "empty" assertProgramEmpty
                                , testCase "single" assertProgramSingle
                                , testCase "extra-ws" assertProgramExtraWS
                                , testCase "embedded" assertProgramEmbedded
                                , testCase "multiple" assertProgramMultiple
                                ]
    ]

