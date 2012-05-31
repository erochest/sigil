{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid
import qualified Data.List as L
import qualified Data.Text as T

-- TODO: make exec the function associated with a type class.

execq :: Quote -> Stack -> Stack
execq q s = foldl (flip exec) s q

exec :: SWord -> Stack -> Stack
exec b@(B _) (Stack ss) = Stack (b:ss)
exec i@(I _) (Stack ss) = Stack (i:ss)
exec   (S s) ss         = s `execOp` ss
exec q@(Q _) (Stack ss) = Stack (q:ss)

execOp :: Symbol -> Stack -> Stack
execOp "dip" (Stack ((Q q) : i : ss)) = Stack [i] `mappend` execq q (Stack ss)
execOp "dup" (Stack (s:ss))           = Stack (s:s:ss)
execOp "pop" (Stack (_:ss))           = Stack ss
execOp "swap" (Stack (x:y:ss))        = Stack (y:x:ss)
execOp "call" (Stack ((Q q):ss))      = q `execq` Stack ss
execOp "quote" (Stack (s:ss))         = Stack (Q [s] : ss)
execOp "compose" (Stack (s2:s1:ss))   = Stack (s1 `mappend` s2 : ss)
execOp "curry" (Stack (Q q:i:ss))     = Stack (Q (i:q) : ss)
execOp "rot" (Stack (i1:i2:i3:ss))    = Stack (i3:i1:i2:ss)
execOp "bi" (Stack (Q q1:Q q2:i:ss))  = Stack (p1:p2:ss)
    where (Stack (p1:_)) = q1 `execq` Stack [i]
          (Stack (p2:_)) = q2 `execq` Stack [i]

execOp "+" (Stack (I i:I j:ss))       = Stack (I (i + j):ss)
execOp "*" (Stack (I i:I j:ss))       = Stack (I (i * j):ss)

execOp _ s                            = s

testWord :: SWord
testWord = Q [I 42, B True, S "hi"]

testStack2 :: Stack
testStack2 = Stack [I 42]

testStack3 :: Stack
testStack3 = Stack [I 42, B True, S "hi"]

-- So this will compile
main :: IO ()
main = putStrLn "sigil"

