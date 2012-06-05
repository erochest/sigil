{-# LANGUAGE OverloadedStrings #-}

module Test.Sigil.Exec
    ( execTests
    ) where

import           Language.Sigil
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Text.Printf

--

assertCode :: String -> SWord -> Quote -> Assertion
assertCode msg program expected = do
    actual <- fst `fmap` runSigilCode defaultEnv program
    let msg' = printf "%s: %s => %s" msg (show program) (show actual)
    assertBool msg' $ Stack expected == actual

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

assertStackDip :: Assertion
assertStackDip = do
    ac (Q [I 1, I 2, I 3, Q [S "+"], S "dip"])   [I 3, I 3]
    ac (Q [I 1, I 2, I 3, Q [S "pop"], S "dip"]) [I 3, I 1]
    where ac = assertCode "assertStackDip"

assertStackDup :: Assertion
assertStackDup = do
    ac (Q [I 1, I 2, I 3, S "dup"])   [I 3, I 3, I 2, I 1]
    ac (Q [B False, B True, S "dup"]) [B True, B True, B False]
    where ac = assertCode "assertStackDup"

assertStackPop :: Assertion
assertStackPop = do
    ac (Q [I 1, I 2, S "pop"])        [I 1]
    ac (Q [B False, B True, S "pop"]) [B False]
    where ac = assertCode "assertStackPop"

assertStackSwap :: Assertion
assertStackSwap = do
    ac (Q [I 1, I 2, S "swap"])        [I 1, I 2]
    ac (Q [B False, B True, S "swap"]) [B False, B True]
    where ac = assertCode "assertStackSwap"

assertStackCall :: Assertion
assertStackCall = do
    ac (Q [I 1, I 2, Q [S "+"], S "call"])   [I 3]
    ac (Q [I 1, I 2, Q [S "pop"], S "call"]) [I 1]
    where ac = assertCode "assertStackCall"

assertStackQuote :: Assertion
assertStackQuote = do
    ac (Q [I 1, I 2, S "quote"])        [Q [I 2], I 1]
    ac (Q [B False, B True, S "quote"]) [Q [B True], B False]
    where ac = assertCode "assertStackQuote"

assertStackCompose :: Assertion
assertStackCompose = do
    ac (Q [I 5, Q [S "+"], S "compose"])         [Q [I 5, S "+"]]
    ac (Q [Q [S "pop"], Q [S "+"], S "compose"]) [Q [S "pop", S "+"]]
    where ac = assertCode "assertStackCompose"

assertStackCurry :: Assertion
assertStackCurry = do
    ac (Q [I 5, Q [S "+"], S "curry"])       [Q [I 5, S "+"]]
    where ac = assertCode "assertStackCurry"

assertStackRot :: Assertion
assertStackRot = do
    ac (Q [I 1, I 2, I 3, S "rot"])           [I 1, I 3, I 2]
    ac (Q [B False, B True, B True, S "rot"]) [B False, B True, B True]
    where ac = assertCode "assertStackRot"

assertStackBi :: Assertion
assertStackBi = do
    ac (Q [I 2, Q [I 5, S "+"], Q [I 5, S "*"], S "bi"]) [I 10, I 7]
    ac (Q [I 3, Q [I 6, S "+"], Q [I 7, S "*"], S "bi"]) [I 21, I 9]
    where ac = assertCode "assertStackBi"

assertIntPlus :: Assertion
assertIntPlus = do
    ac (Q [I 1, I 2, S "+"])             [I 3]
    ac (Q [I 1, I 2, I 3, S "+", S "+"]) [I 6]
    where ac = assertCode "assertIntPlus"

assertIntStar :: Assertion
assertIntStar = do
    ac (Q [I 2, I 3, S "*"])             [I 6]
    ac (Q [I 2, I 3, I 4, S "*", S "*"]) [I 24]
    where ac = assertCode "assertIntStar"

-- Tests.
execTests :: [Test]
execTests =
    [ testGroup "types"  [ testCase "exec-bool"   assertExecBool
                         , testCase "exec-int"    assertExecInt
                         , testCase "exec-symbol" assertExecSymbol
                         , testCase "exec-quote"  assertExecQuote
                         ]
    , testGroup "stack"  [ testCase "dip"     assertStackDip
                         , testCase "dup"     assertStackDup
                         , testCase "pop"     assertStackPop
                         , testCase "swap"    assertStackSwap
                         , testCase "call"    assertStackCall
                         , testCase "quote"   assertStackQuote
                         , testCase "compose" assertStackCompose
                         , testCase "curry"   assertStackCurry
                         , testCase "rot"     assertStackRot
                         , testCase "bi"      assertStackBi
                         ]
    , testGroup "int"    [ testCase "+" assertIntPlus
                         , testCase "*" assertIntStar
                         ]
    ]

