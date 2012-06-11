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

assertAddInt :: Assertion
assertAddInt = do
    ac (Q [I 1, I 2, S "add_int"])                   [I 3]
    ac (Q [I 1, I 2, I 3, S "add_int", S "add_int"]) [I 6]
    where ac = assertCode "assertAddInt"

assertDec :: Assertion
assertDec = do
    ac [I 4, S "dec"]    [I 3]
    ac [I 42, S "dec"]   [I 41]
    ac [I 1, S "dec"]    [I 0]
    ac [I 0, S "dec"]    [I (-1)]
    ac [I (-1), S "dec"] [I (-2)]
    where ac = assertCode "assertDec" . Q

assertIntStar :: Assertion
assertIntStar = do
    ac (Q [I 2, I 3, S "*"])             [I 6]
    ac (Q [I 2, I 3, I 4, S "*", S "*"]) [I 24]
    where ac = assertCode "assertIntStar"

-- Quote

assertApply :: Assertion
assertApply = do
    ac (Q [I 1, I 2, Q [S "add_int"], S "apply"])   [I 3]
    ac (Q [I 1, I 2, Q [S "pop"], S "apply"]) [I 1]
    where ac = assertCode "assertApply"

assertCompose :: Assertion
assertCompose = do
    ac (Q [I 5, Q [S "add_int"], S "compose"])         [Q [I 5, S "add_int"]]
    ac (Q [Q [S "pop"], Q [S "add_int"], S "compose"]) [Q [S "pop", S "add_int"]]
    where ac = assertCode "assertCompose"

assertCons :: Assertion
assertCons = do
    ac [Q [I 2, I 3], I 1, S "cons"] [Q [I 1, I 2, I 3]]
    where ac = assertCode "assertCons" . Q

-- Stack

assertDip :: Assertion
assertDip = do
    ac (Q [I 1, I 2, I 3, Q [S "add_int"], S "dip"])   [I 3, I 3]
    ac (Q [I 1, I 2, I 3, Q [S "pop"], S "dip"]) [I 3, I 1]
    where ac = assertCode "assertDip"

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

assertStackQuote :: Assertion
assertStackQuote = do
    ac (Q [I 1, I 2, S "quote"])        [Q [I 2], I 1]
    ac (Q [B False, B True, S "quote"]) [Q [B True], B False]
    where ac = assertCode "assertStackQuote"

assertStackCurry :: Assertion
assertStackCurry = do
    ac (Q [I 5, Q [S "add_int"], S "curry"])       [Q [I 5, S "add_int"]]
    where ac = assertCode "assertStackCurry"

assertStackRot :: Assertion
assertStackRot = do
    ac (Q [I 1, I 2, I 3, S "rot"])           [I 1, I 3, I 2]
    ac (Q [B False, B True, B True, S "rot"]) [B False, B True, B True]
    where ac = assertCode "assertStackRot"

assertStackBi :: Assertion
assertStackBi = do
    ac (Q [I 2, Q [I 5, S "add_int"], Q [I 5, S "*"], S "bi"]) [I 10, I 7]
    ac (Q [I 3, Q [I 6, S "add_int"], Q [I 7, S "*"], S "bi"]) [I 21, I 9]
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
    , testGroup "int"           [ testCase "add_int" assertAddInt
                                , testCase "dec"     assertDec
                                , testCase "*" assertIntStar
                                ]
    , testGroup "quote"         [ testCase "apply"   assertApply
                                , testCase "compose" assertCompose
                                , testCase "cons" assertCons
                                ]
    , testGroup "stack"         [ testCase "dip"     assertDip
                                , testCase "dup"     assertStackDup
                                , testCase "pop"     assertStackPop
                                , testCase "swap"    assertStackSwap
                                , testCase "quote"   assertStackQuote
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

