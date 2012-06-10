{-# LANGUAGE OverloadedStrings #-}

module Test.Sigil.Ops
    ( opsTests
    ) where

import Language.Sigil
import Test.HUnit (Assertion, assertBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Utils
import Text.Printf

-- Boolean tests
assertAnd :: Assertion
assertAnd = do
    ac [B True,  B True,  S "and"] [B True]
    ac [B True,  B False, S "and"] [B False]
    ac [B False, B True,  S "and"] [B False]
    ac [B False, B False, S "and"] [B False]
    where ac = assertCode "assertAnd" . Q

assertOr :: Assertion
assertOr = do
    ac [B True,  B True,  S "or"] [B True]
    ac [B True,  B False, S "or"] [B True]
    ac [B False, B True,  S "or"] [B True]
    ac [B False, B False, S "or"] [B False]
    where ac = assertCode "assertOr" . Q

assertNot :: Assertion
assertNot = do
    ac [B True,  S "not"] [B False]
    ac [B False, S "not"] [B True]
    where ac = assertCode "assertNot" . Q

-- Integer tests

assertIntPlus :: Assertion
assertIntPlus = do
    ac (Q [I 1, I 2, S "add_int"])                   [I 3]
    ac (Q [I 1, I 2, I 3, S "add_int", S "add_int"]) [I 6]
    where ac = assertCode "assertIntPlus"

assertIntStar :: Assertion
assertIntStar = do
    ac (Q [I 2, I 3, S "*"])             [I 6]
    ac (Q [I 2, I 3, I 4, S "*", S "*"]) [I 24]
    where ac = assertCode "assertIntStar"

-- Stack

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

--

opsTests :: [Test]
opsTests =
    [ testGroup "bool"          [ testCase "and" assertAnd
                                , testCase "or"  assertOr
                                , testCase "not" assertNot
                                ]
    , testGroup "number"        [
                                ]
    , testGroup "double"        [
                                ]
    , testGroup "int"           [ testCase "add_int" assertIntPlus
                                , testCase "*" assertIntStar
                                ]
    , testGroup "quote"         [
                                ]
    , testGroup "stack"         [ testCase "dip"     assertStackDip
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
    , testGroup "symbol"        [
                                ]
    , testGroup "vector bool"   [
                                ]
    , testGroup "vector int"    [
                                ]
    , testGroup "vector double" [
                                ]
    , testGroup "noop"          [
                                ]
    ]

