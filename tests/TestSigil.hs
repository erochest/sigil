
module Main where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Sigil.Exec
import Test.Sigil.Parser
import Test.Sigil.Types

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "exec"   execTests
    , testGroup "parser" parserTests
    , testGroup "types"  typeTests
    ]

