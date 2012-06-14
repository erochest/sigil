{-# LANGUAGE OverloadedStrings #-}

module Test.Sigil.Ops
    ( opsTests
    ) where

import           Language.Sigil
import           Test.HUnit (Assertion)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Utils

-- Generic tests
assertEq :: Assertion
assertEq = do
    ac [B True,     B True,      S "eq"] [B True]
    ac [B True,     B False,     S "eq"] [B False]
    ac [I 42,       I 42,        S "eq"] [B True]
    ac [I 42,       I 13,        S "eq"] [B False]
    ac [F 3.141,    F 3.141,     S "eq"] [B True]
    ac [F 3.141,    F 1.0,       S "eq"] [B False]
    ac [Q [S "hi"], Q [S "hi"],  S "eq"] [B True]
    ac [Q [S "hi"], Q [S "bye"], S "eq"] [B False]
    where ac = assertCode "assertEq" . Q

-- Boolean tests
assertAnd :: Assertion
assertAnd = do
    ac [B True,  B True,  S "and"] [B True]
    ac [B True,  B False, S "and"] [B False]
    ac [B False, B True,  S "and"] [B False]
    ac [B False, B False, S "and"] [B False]
    where ac = assertCode "assertAnd" . Q

assertFalse :: Assertion
assertFalse = do
    ac [S "false"] [B False]
    where ac = assertCode "assertFalse" . Q

assertIf :: Assertion
assertIf = do
    ac [S "false", Q [I 13], Q [I 42], S "if"] [I 42]
    ac [B True,    Q [I 13], Q [I 42], S "if"] [I 13]
    where ac = assertCode "assertIf" . Q

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

assertDivInt :: Assertion
assertDivInt = do
    ac [I 2, I 4, S "div_int"] [I 2]
    ac [I 1, I 4, S "div_int"] [I 4]
    ac [I 4, I 1, S "div_int"] [I 0]
    where ac = assertCode "assertDivInt" . Q

assertInc :: Assertion
assertInc = do
    ac [I (-3), S "inc"] [I (-2)]
    ac [I (-1), S "inc"] [I 0]
    ac [I 0,    S "inc"] [I 1]
    ac [I 1,    S "inc"] [I 2]
    ac [I 4,    S "inc"] [I 5]
    ac [I 15,   S "inc"] [I 16]
    where ac = assertCode "assertInc" . Q

assertModInt :: Assertion
assertModInt = do
    ac [I 1, I 4, S "mod_int"] [I 0]
    ac [I 2, I 4, S "mod_int"] [I 0]
    ac [I 3, I 4, S "mod_int"] [I 1]
    ac [I 4, I 4, S "mod_int"] [I 0]
    ac [I 5, I 4, S "mod_int"] [I 4]
    where ac = assertCode "assertModInt" . Q

assertMulInt :: Assertion
assertMulInt = do
    ac [I (-1), I 4, S "mul_int"] [I (-4)]
    ac [I 0,    I 4, S "mul_int"] [I 0]
    ac [I 1,    I 4, S "mul_int"] [I 4]
    ac [I 2,    I 4, S "mul_int"] [I 8]
    ac [I 3,    I 4, S "mul_int"] [I 12]
    ac [I 4,    I 4, S "mul_int"] [I 16]
    ac [I 5,    I 4, S "mul_int"] [I 20]
    where ac = assertCode "assertMulInt" . Q

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

assertEmpty :: Assertion
assertEmpty = do
    ac [Q [], S "empty"]            [B True]
    ac [Q [I 1], S "empty"]         [B False]
    ac [Q [I 5, S "+"], S "empty"]  [B False]
    where ac = assertCode "assertEmpty" . Q

assertList :: Assertion
assertList = do
    ac [I 42,                  S "list"] [Q [I 42]]
    ac [S "false",             S "list"] [Q [B False]]
    ac [I 1, I 2, S "add_int", S "list"] [Q [I 3]]
    where ac = assertCode "assertList" . Q

assertQuote :: Assertion
assertQuote = do
    ac [I 42,                  S "quote"] [Q [I 42]]
    ac [S "false",             S "quote"] [Q [B False]]
    ac [I 1, I 2, S "add_int", S "quote"] [Q [I 3]]
    where ac = assertCode "assertQuote" . Q

-- Stack

assertDip :: Assertion
assertDip = do
    ac (Q [I 1, I 2, I 3, Q [S "add_int"], S "dip"])   [I 3, I 3]
    ac (Q [I 1, I 2, I 3, Q [S "pop"], S "dip"]) [I 3, I 1]
    where ac = assertCode "assertDip"

assertDup :: Assertion
assertDup = do
    ac (Q [I 1, I 2, I 3, S "dup"])   [I 3, I 3, I 2, I 1]
    ac (Q [B False, B True, S "dup"]) [B True, B True, B False]
    where ac = assertCode "assertDup"

assertPApply :: Assertion
assertPApply = do
    ac [I 5, Q [S "add_int"], S "papply"] [Q [I 5, S "add_int"]]
    where ac = assertCode "assertPApply" . Q

assertPop :: Assertion
assertPop = do
    ac (Q [I 1, I 2, S "pop"])        [I 1]
    ac (Q [B False, B True, S "pop"]) [B False]
    where ac = assertCode "assertPop"

assertStackSwap :: Assertion
assertStackSwap = do
    ac (Q [I 1, I 2, S "swap"])        [I 1, I 2]
    ac (Q [B False, B True, S "swap"]) [B False, B True]
    where ac = assertCode "assertStackSwap"

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
    [ testGroup "generic"       [ testCase "eq" assertEq
                                ]
    , testGroup "bool"          [ testCase "and"   assertAnd
                                , testCase "false" assertFalse
                                , testCase "if"    assertIf
                                , testCase "or"    assertOr
                                , testCase "not"   assertNot
                                ]
    , testGroup "number"        [
                                ]
    , testGroup "double"        [
                                ]
    , testGroup "int"           [ testCase "add_int" assertAddInt
                                , testCase "dec"     assertDec
                                , testCase "div_int" assertDivInt
                                , testCase "inc"     assertInc
                                , testCase "mod_int" assertModInt
                                , testCase "mul_int" assertMulInt
                                ]
    , testGroup "quote"         [ testCase "apply"   assertApply
                                , testCase "compose" assertCompose
                                , testCase "cons"    assertCons
                                , testCase "empty"   assertEmpty
                                , testCase "list"    assertList
                                , testCase "quote"   assertQuote
                                ]
    , testGroup "stack"         [ testCase "dip"     assertDip
                                , testCase "dup"     assertDup
                                , testCase "papply"  assertPApply
                                , testCase "pop"     assertPop

                                , testCase "swap"    assertStackSwap
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

