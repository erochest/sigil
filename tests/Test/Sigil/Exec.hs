{-# LANGUAGE OverloadedStrings #-}

module Test.Sigil.Exec
    ( execTests
    ) where

import           Language.Sigil
import           Test.HUnit (Assertion)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Utils

--

assertExecBool :: Assertion
assertExecBool = do
    ac (B False)             [B False]
    ac (B True)              [B True]
    al (Q [B True, B False]) [B False, B True]
    where ac = assertCode "assertExecBool"
          al = assertCode "assertExecBool"

assertExecInt :: Assertion
assertExecInt = do
    ac (I 42)                 [I 42]
    ac (I 13)                 [I 13]
    ac (I 14)                 [I 14]
    al (Q [I 13, I 14, I 42]) [I 42, I 14, I 13]
    where ac = assertCode "assertExecInt"
          al = assertCode "assertExecInt"

assertExecSymbol :: Assertion
assertExecSymbol = do
    -- Symbols execute by attempting to execute their functions. If they are
    -- not valid to execute on the current stack, they are no-ops.
    ac (S "+")                      []
    ac (S "pop")                    []
    ac (S "hi")                     []
    al (Q [S "+", S "pop", S "hi"]) []
    where ac = assertCode "assertExecSymbol"
          al = assertCode "assertExecSymbol"

assertExecQuote :: Assertion
assertExecQuote = do
    ac (Q [Q [I 1, I 2, I 3]]) [Q [I 1, I 2, I 3]]
    ac (Q [Q []])              [Q []]
    ac (Q [Q [S "hi"]])        [Q [S "hi"]]
    al (Q [Q [], Q [I 4]])     [Q [I 4], Q []]
    where ac = assertCode "assertExecQuote"
          al = assertCode "assertExecQuote"

assertExecSucc :: Assertion
assertExecSucc = do
    ad [I 3] "\ndefine succ\n{ 1 + }\n\n2 succ\n"
    where ad = assertText "assertExecSucc"

-- Tests.
execTests :: [Test]
execTests =
    [ testGroup "types"     [ testCase "exec-bool"   assertExecBool
                            , testCase "exec-int"    assertExecInt
                            , testCase "exec-symbol" assertExecSymbol
                            , testCase "exec-quote"  assertExecQuote
                            ]
    , testGroup "defines"   [ testCase "succ" assertExecSucc
                            ]
    ]

