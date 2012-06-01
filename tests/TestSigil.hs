
module Main where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Sigil.Types
import Test.Sigil.Exec

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "types" typeTests
    , testGroup "exec"  execTests
    ]

