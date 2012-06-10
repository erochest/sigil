
module Test.Utils
    ( assertCode
    ) where

import           Language.Sigil
import           Test.HUnit (Assertion, assertBool)
import           Text.Printf

assertCode :: String -> SWord -> Quote -> Assertion
assertCode msg program expected = do
    actual <- fst `fmap` runSigilCode defaultEnv program
    let msg' = printf "%s: %s => %s" msg (show program) (show actual)
    assertBool msg' $ Stack expected == actual

