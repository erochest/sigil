{-# LANGUAGE OverloadedStrings #-}

module Language.Sigil.Exec
    (
    ) where

import           Data.Monoid
import qualified Data.Text as T
import           Language.Sigil.Types

instance Code a => Code [a] where
    exec q s = foldl (flip exec) s q

instance Code SWord where
    exec (S s) ss         = s `exec` ss
    exec x     (Stack xs) = Stack (x:xs)

instance Code T.Text where
    exec "dip" (Stack ((Q q) : i : ss)) = Stack [i] `mappend` exec q (Stack ss)
    exec "dup" (Stack (s:ss)) = Stack (s:s:ss)
    exec "pop" (Stack (_:ss)) = Stack ss
    exec "swap" (Stack (x:y:ss)) = Stack (y:x:ss)
    exec "call" (Stack ((Q q):ss)) = q `exec` Stack ss
    exec "quote" (Stack (s:ss)) = Stack (Q [s] : ss)
    exec "compose" (Stack (s2:s1:ss)) = Stack (s1 `mappend` s2 : ss)
    exec "curry" (Stack (Q q:i:ss)) = Stack (Q (i:q) : ss)
    exec "rot" (Stack (a:b:c:ss)) = Stack (c:a:b:ss)
    exec "bi" (Stack (Q q1: Q q2 : i : ss)) = Stack (p1:p2:ss)
        where (Stack (p1:_)) = q1 `exec` istack
              (Stack (p2:_)) = q2 `exec` istack
              istack         = Stack [i]

    exec "+" (Stack (I i1 : I i2 : ss)) = Stack (I (i1 + i2) : ss)
    exec "*" (Stack (I i1 : I i2 : ss)) = Stack (I (i1 * i2) : ss)

    exec _ s = s


