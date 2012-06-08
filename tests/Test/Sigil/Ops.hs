{-# LANGUAGE OverloadedStrings #-}

module Test.Sigil.Ops
    ( opsTests
    ) where

import Language.Sigil
import Test.HUnit (Assertion, assertBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Text.Printf

--

--

opsTests :: [Test]
opsTests =
    [ testGroup "bool"          [
                                ]
    , testGroup "double"        [
                                ]
    , testGroup "int"           [
                                ]
    , testGroup "quote"         [
                                ]
    , testGroup "stack"         [
                                ]
    , testGroup "symbol"        [
                                ]
    , testGroup "vector bool"   [
                                ]
    , testGroup "vector int"    [
                                ]
    , testGroup "vector double" [
                                ]
    ]

