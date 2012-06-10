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
import           Language.Sigil.Ops
import           Language.Sigil.Types

instance Code a => Code [a] where
    exec q s = foldM (flip exec) s q

instance Code SWord where
    exec (S s) ss         = s `exec` ss
    exec x     (Stack xs) = return $ Stack (x:xs)

instance Code T.Text where
    exec = op

    {-
    exec "dip"     (Stack ((Q q) : i : ss))      = mappend (Stack [i]) `fmap` exec q (Stack ss)
    exec "dup"     (Stack (s:ss))                = return $ Stack (s:s:ss)
    exec "pop"     (Stack (_:ss))                = return $ Stack ss
    exec "swap"    (Stack (x:y:ss))              = return $ Stack (y:x:ss)
    exec "call"    (Stack ((Q q):ss))            = q `exec` Stack ss
    exec "quote"   (Stack (s:ss))                = return $ Stack (Q [s] : ss)
    exec "compose" (Stack (s2:s1:ss))            = return $ Stack (s1 `mappend` s2 : ss)
    exec "curry"   (Stack (Q q:i:ss))            = return $ Stack (Q (i:q) : ss)
    exec "rot"     (Stack (a:b:c:ss))            = return $ Stack (c:a:b:ss)
    exec "bi"      (Stack (Q q1: Q q2 : i : ss)) = do
        Stack (p1:_) <- q1 `exec` istack
        Stack (p2:_) <- q2 `exec` istack
        return $ Stack (p1:p2:ss)
        where istack         = Stack [i]

    exec "+" (Stack (I i1 : I i2 : ss)) = return $ Stack (I (i1 + i2) : ss)
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

