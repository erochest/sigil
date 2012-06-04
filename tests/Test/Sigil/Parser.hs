{-# LANGUAGE OverloadedStrings #-}

module Test.Sigil.Parser
    ( parserTests
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
import           Text.Printf

-- Utilities

assertLeft :: String -> T.Text -> Assertion
assertLeft msg input =
    assertBool msg' $ either (const True) (const False) actual
    where msg'   = printf "%s %s => %s" msg (show input) (show actual)
          actual = parseText input

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

{-
 - assertLiteralFloat :: Assertion
 - assertLiteralFloat = do
 -     a 2.718281 "2.718281"
 -     a 3.141592 "3.141592"
 -     a 0.33     "0.33"
 -     a (-0.33)  "-0.33"
 -     where
 -         a :: Double -> T.Text -> Assertion
 -         a = assertParser "assertLiteralFloat" . F
 -}

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

{-
 - assertInstructionAlpha :: Assertion
 - assertInstructionAlpha = do
 -     a "aaa" "aaa"
 -     a "bbb" "BBB"
 -     a "ccc" "CcC"
 -     a "abcdefghijklmnopqrstuvwxyz" "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 -     where
 -         a :: T.Text -> T.Text -> Assertion
 -         a = assertParse "assertInstructionAlpha"
 - 
 - assertInstructionAlphaNum :: Assertion
 - assertInstructionAlphaNum = do
 -     a "a888" "A888"
 -     a "g9h0i1" "G9h0I1"
 -     where
 -         a :: T.Text -> T.Text -> Assertion
 -         a = assertParse "assertInstructionAlphaNum"
 - 
 - assertInstructionSymbol :: Assertion
 - assertInstructionSymbol = do
 -     a "a+9" "A+9"
 -     a "exec.do*times" "EXEC.DO*TIMES"
 -     a "float.%" "FLOAT.%"
 -     a "float.*" "FLOAT.*"
 -     a "float.+" "FLOAT.+"
 -     a "float.-" "FLOAT.-"
 -     a "float./" "FLOAT./"
 -     a "float.<" "FLOAT.<"
 -     a "float.=" "FLOAT.="
 -     a "float.>" "FLOAT.>"
 -     where
 -         a :: T.Text -> T.Text -> Assertion
 -         a = assertParse "assertInstructionSymbol"
 - 
 - assertInstructionsDot :: Assertion
 - assertInstructionsDot = do
 -     a "a.+" "A.+"
 -     a "integer.+" "INTEGER.+"
 -     a "boolean.dup" "BOOLEAN.DUP"
 -     where
 -         a :: T.Text -> T.Text -> Assertion
 -         a = assertParse "assertInstructionsDot"
 - 
 - assertProgramEmpty :: Assertion
 - assertProgramEmpty = do
 -     a "()"
 -     a "( )"
 -     a "(\n)"
 -     where a = assertParse "assertProgramEmpty"
 - 
 - assertProgramSingle :: Assertion
 - assertProgramSingle = do
 -     a [B True]        "(true)"
 -     a [F 3.14]        "(3.14)"
 -     a [I 42]          "(42)"
 -     a [N "integer.+"] "(INTEGER.+)"
 -     where a = assertParse "assertProgramSingle"
 - 
 - assertProgramExtraWS :: Assertion
 - assertProgramExtraWS = do
 -     a [B True]        "( true )"
 -     a [F 3.14]        "( 3.14 )"
 -     a [I 42]          "( 42 )"
 -     a [N "integer.+"] "( INTEGER.+ )"
 -     where a = assertParse "assertProgramExtraWS"
 - 
 - assertProgramEmbedded :: Assertion
 - assertProgramEmbedded = do
 -     a [C [B True]] "( (TRUE) )"
 -     a [C [I 42  ]] "( ( 42 ) )"
 -     a [C [C [F 3.141592]]] "(((3.141592)) )"
 -     where a = assertParse "assertProgramEmbedded"
 - 
 - assertProgramMultiple :: Assertion
 - assertProgramMultiple = do
 -     a [I 42, I 13, F 3.141592] "( 42 13 3.141592 )"
 -     a [I 42, C [I 13, F 3.141592], N "integer.+"] "(42 (13 3.141592) INTEGER.+)"
 -     a [C [N "a", N "b"], C [N "d", N "e"], C [N "f", N "g"]]
 -            "( (a b) ( d e ) (f g) )"
 -     where a = assertParse "assertProgramMultiple"
 -}

--

parserTests :: [Test]
parserTests =
    [ testGroup "literals"      [ testCase "boolean" assertLiteralBool
                                -- , testCase "float" assertLiteralFloat
                                , testCase "integer" assertLiteralInt
                                ]
    , testGroup "instructions"  [
                                -- testCase "alpha" assertInstructionAlpha
                                -- , testCase "alphanum" assertInstructionAlphaNum
                                -- , testCase "symbol" assertInstructionSymbol
                                -- , testCase "dot" assertInstructionDot
                                ]
    , testGroup "programs"      [
                                -- testCase "empty" assertProgramEmpty
                                -- , testCase "single" assertProgramSingle
                                -- , testCase "extra-ws" assertProgramExtraWS
                                -- , testCase "embedded" assertProgramEmbedded
                                -- , testCase "multiple" assertProgramMultiple
                                ]
    ]

