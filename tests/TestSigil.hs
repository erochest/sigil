
module Main where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Sigil.Types

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "types" typeTests
    ]

