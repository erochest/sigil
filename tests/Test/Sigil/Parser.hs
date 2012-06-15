{-# LANGUAGE OverloadedStrings #-}

module Test.Sigil.Parser
    ( parserTests
    ) where

import qualified Data.Text as T
-- import qualified Data.Vector.Unboxed as V
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
assertParse msg expected input = assertBool msg' $ expected' == actual
    where msg'      = printf "%s %s => %s /= %s"
                             msg (show input) (show expected) (show actual)
          expected' = Right (Program [] [expected])
          actual    = parseText input

assertDefine :: String -> Symbol -> Quote -> T.Text -> Assertion
assertDefine msg name define input = assertBool msg' $ expected' == actual
    where msg'      = printf "%s %s =>   define %s -> %s /= %s"
                             msg (show input) (show name) (show define)
                             (show actual)
          expected' = Right (Program [Define name define] [])
          actual    = parseText input

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
 -         a = assertParse "assertLiteralFloat" . F
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
 - assertLiteralVInt :: Assertion
 - assertLiteralVInt = do
 -     a V.empty                             "#i< >"
 -     a (V.fromList [1])                    "#i< 1 >"
 -     a (V.fromList [1, 2, 3])              "#i< 1 2 3 >"
 -     a (V.fromList [13, 42, 74, 100, 121]) "#i< 13 42 74 100 121 >"
 -     where a :: (V.Vector Int) -> T.Text -> Assertion
 -           a = assertParse "assertLiteralVInt" . VI
 - 
 - assertLiteralVFloat :: Assertion
 - assertLiteralVFloat = do
 -     a V.empty                                "#f< >"
 -     a (V.fromList [1.0])                     "#f< 1.0 >"
 -     a (V.fromList [-1.0, 0.0, 1.0])          "#f< -1.0 0.0 1.0 >"
 -     a (V.fromList [1.4142, 2.71828, 3.1415]) "#f< 1.4142 2.71828 3.1415 >"
 -     where a :: (V.Vector Double) -> T.Text -> Assertion
 -           a = assertParse "assertLiteralVFloat" . VF
 - 
 - assertLiteralVBool :: Assertion
 - assertLiteralVBool = do
 -     a V.empty "#b<>"
 -     a (V.fromList [True]) "#b<1>"
 -     a (V.fromList [False]) "#b<0>"
 -     a (V.fromList [True, False]) "#b<10>"
 -     a (V.fromList [False, False, True]) "#b<001>"
 -     where a :: (V.Vector Bool) -> T.Text -> Assertion
 -           a = assertParse "assertLiteralVBool" . VB
 -}

assertInstructionAlpha :: Assertion
assertInstructionAlpha = do
    a "aaa"                         "aaa"
    a "BBB"                         "BBB"
    a "CcC"                         "CcC"
    a "ABCDEFGHIJKLMNOPQRSTUVWXYZ"  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    where
        a :: T.Text -> T.Text -> Assertion
        a = assertParse "assertInstructionAlpha" . S

assertInstructionAlphaNum :: Assertion
assertInstructionAlphaNum = do
    a "A888"    "A888"
    a "G9h0I1"  "G9h0I1"
    where
        a :: T.Text -> T.Text -> Assertion
        a = assertParse "assertInstructionAlphaNum" . S

assertInstructionUnderscore :: Assertion
assertInstructionUnderscore = do
    a "_"    "_"
    a "h_1"  "h_1"
    where a = assertParse "assertInstructionUnderscore" . S

assertInstructionSymbol :: Assertion
assertInstructionSymbol = do
    mapM_ (\input -> a input input) [ "-"
                                    , "`"
                                    , "!"
                                    , "@"
                                    , "#"
                                    , "$"
                                    , "+"
                                    , "*"
                                    , "%"
                                    , "/"
                                    , "<"
                                    , "="
                                    , ">"
                                    ]
    where
        a :: T.Text -> T.Text -> Assertion
        a = assertParse "assertInstructionSymbol" . S

assertInstructionParens :: Assertion
assertInstructionParens = do
    as "("                   "("
    as "()"                  "()"
    as ")"                   ")"
    as ","                   ","
    aq [S "(", S ")"]        "[ ( ) ]"
    aq [S "()"]              "[ () ]"
    aq [S "(", S ",", S ")"] "[ ( , ) ]"
    aq [S "(,)"]             "[ (,) ]"
    where as = assertParse "assertInstructionParens" . S
          aq = assertParse "assertInstructionParens" . Q

assertInstructionMisc :: Assertion
assertInstructionMisc = do
    mapM_ (\input -> a input input) [ "thisisaverylongname"
                                    , "q"
                                    , "n42"
                                    , "is-big?"
                                    , "++"
                                    , "<html>"
                                    , "("
                                    , "=^,,^="
                                    ]
    where a = assertParse "assertInstructionMisc" . S


assertProgramEmpty :: Assertion
assertProgramEmpty = do
    a "[]"
    a "[ ]"
    a "[\n]"
    where a = assertParse "assertProgramEmpty" (Q [])

assertProgramSingle :: Assertion
assertProgramSingle = do
    a (Q [B True])        "[#t]"
    -- a (Q [F 3.14])        "[3.14]"
    a (Q [I 42])          "[42]"
    a (Q [S "+"]) "[+]"
    where a = assertParse "assertProgramSingle"

assertProgramExtraWS :: Assertion
assertProgramExtraWS = do
    a (Q [B True])        "[ #t ]"
    -- a (Q [F 3.14])        "[ 3.14 ]"
    a (Q [I 42])          "[ 42 ]"
    a (Q [S "+"]) "[ + ]"
    where a = assertParse "assertProgramExtraWS"

assertProgramEmbedded :: Assertion
assertProgramEmbedded = do
    a (Q [Q [B True]])         "[ [#t] ]"
    a (Q [Q [I 42  ]])         "[ [ 42 ] ]"
    -- a (Q [Q [Q [F 3.141592]]]) "[[[3.141592]] ]"
    where a = assertParse "assertProgramEmbedded"

assertProgramMultiple :: Assertion
assertProgramMultiple = do
    a (Q [I 42, I 13]) "[ 42 13 ]"
    -- a (Q [I 42, I 13, F 3.141592]) "[ 42 13 3.141592 ]"
    a (Q [I 42, Q [I 13], S "+"]) "[42 [13] +]"
    -- a (Q [I 42, Q [I 13, F 3.141592], S "+"]) "[42 [13 3.141592] +]"
    a (Q [Q [S "a", S "b"], Q [S "d", S "e"], Q [S "f", S "g"]])
      "[ [a b] [ d e ] [f g] ]"
    where a = assertParse "assertProgramMultiple"

assertDefineSucc :: Assertion
assertDefineSucc = do
    ad "succ" [I 1, S "+"] "define succ\n{ 1 + }\n"
    where ad = assertDefine "assertDefineSucc"

--

parserTests :: [Test]
parserTests =
    [ testGroup "literals"      [ testCase "boolean" assertLiteralBool
                                -- , testCase "float" assertLiteralFloat
                                , testCase "integer" assertLiteralInt
                                -- , testCase "vector-int" assertLiteralVInt
                                -- , testCase "vector-float" assertLiteralVFloat
                                -- , testCase "vector-bool" assertLiteralVBool
                                ]
    , testGroup "instructions"  [ testCase "alpha"      assertInstructionAlpha
                                , testCase "alphanum"   assertInstructionAlphaNum
                                , testCase "underscore" assertInstructionUnderscore
                                , testCase "symbol"     assertInstructionSymbol
                                , testCase "parens"     assertInstructionParens
                                , testCase "misc"       assertInstructionMisc
                                ]
    , testGroup "programs"      [ testCase "empty" assertProgramEmpty
                                , testCase "single" assertProgramSingle
                                , testCase "extra-ws" assertProgramExtraWS
                                , testCase "embedded" assertProgramEmbedded
                                , testCase "multiple" assertProgramMultiple
                                ]
    , testGroup "defines"       [ testCase "succ" assertDefineSucc
                                ]
    ]

