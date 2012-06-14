{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Sigil.Exec
    ( runSigil
    , runSigilEnv
    , runSigilCode
    , defaultEnv
    ) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Monoid
import qualified Data.Text as T
import           Language.Sigil.Types

instance Code a => Code [a] where
    exec q s = foldM (flip exec) s q

instance Code SWord where
    exec (S s) ss         = s `exec` ss
    exec x     (Stack xs) = return $ Stack (x:xs)

instance Code T.Text where
    exec = op

    {-
    exec "pop"     (Stack (_:ss))                = return $ Stack ss
    exec "swap"    (Stack (x:y:ss))              = return $ Stack (y:x:ss)
    exec "call"    (Stack ((Q q):ss))            = q `exec` Stack ss
    exec "quote"   (Stack (s:ss))                = return $ Stack (Q [s] : ss)
    exec "curry"   (Stack (Q q:i:ss))            = return $ Stack (Q (i:q) : ss)
    exec "rot"     (Stack (a:b:c:ss))            = return $ Stack (c:a:b:ss)
    exec "bi"      (Stack (Q q1: Q q2 : i : ss)) = do
        Stack (p1:_) <- q1 `exec` istack
        Stack (p2:_) <- q2 `exec` istack
        return $ Stack (p1:p2:ss)
        where istack         = Stack [i]

    exec "*" (Stack (I i1 : I i2 : ss)) = return $ Stack (I (i1 * i2) : ss)

    exec _ s = return s
    -}

defaultEnv :: SigilEnv
defaultEnv = SigilEnv Nothing

-- | This executes a random program against the default environment.
runSigil :: IO SigilOutput
runSigil = runSigilCode defaultEnv mempty

-- | This executes a random program against the environment.
runSigilEnv :: SigilEnv -> IO SigilOutput
runSigilEnv env = runSigilCode env mempty

runSigilCode :: SigilEnv -> SWord -> IO SigilOutput
runSigilCode env word =
    runStateT (run' word mempty) $ env { envProgram = Just word }
    where
        run' :: SWord -> Stack -> Sigil Stack
        run' (Q q) stack = exec q stack
        run' code  stack = exec code stack

op :: T.Text -> StackTransformer

-- Generic operations
op "eq" (Stack (a:b:ss)) = return $ Stack (B (a == b):ss)

-- Stack operations
op "pop"     (Stack (_:ss))       = return $ Stack ss
op "compose" (Stack (a:b:ss))     = return $ Stack (b `mappend` a:ss)
op "dip"     (Stack ((Q q):i:ss)) = mappend (Stack [i]) `fmap` exec q (Stack ss)
op "dup"     (Stack (s:ss))       = return $ Stack (s:s:ss)

-- Boolean operations
op "and"   (Stack (B a:B b:ss))         = return $ Stack (B (a && b):ss)
op "false" (Stack ss)                   = return $ Stack (B False:ss)
op "if"    (Stack (Q _:Q t:B True: ss)) = exec t $ Stack ss
op "if"    (Stack (Q e:Q _:B False:ss)) = exec e $ Stack ss
op "or"    (Stack (B a:B b:ss))         = return $ Stack (B (a || b):ss)
op "not"   (Stack (B a:ss))             = return $ Stack (B (not a) :ss)

-- Integer operations
op "add_int" (Stack (I a:I b:ss)) = return $ Stack (I (a + b):ss)
op "dec"     (Stack (I a:ss))     = return $ Stack (I (a - 1):ss)
op "div_int" (Stack (I a:I b:ss)) = return $ Stack (I (a `div` b):ss)
op "inc"     (Stack (I i:ss))     = return $ Stack (I (i + 1):ss)

-- Double operations

-- Symbol operations

-- Vector operations

-- Quote operations
op "apply" (Stack (Q q:ss))   = q `exec` Stack ss
op "cons"  (Stack (a:Q q:ss)) = return $ Stack (Q (a:q):ss)
op "empty" (Stack (Q []:ss))  = return $ Stack (B True: ss)
op "empty" (Stack (Q _ :ss))  = return $ Stack (B False:ss)
op "list"  (Stack (s:ss))     = return $ Stack (Q [s]:ss)

-- Finally, a no-op.
op _ s = return s

