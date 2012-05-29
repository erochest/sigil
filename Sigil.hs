{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid
import qualified Data.List as L
import qualified Data.Text as T

type Symbol = T.Text

-- | Types for values/words.
type Quote = [GpWord]

data GpWord
    = B Bool
    | I Int
    | S Symbol
    | Q Quote

instance Monoid GpWord where
    mempty = Q []

    mappend (Q []) b      = b
    mappend a      (Q []) = a
    mappend (Q qa) (Q qb) = Q (qa `mappend` qb)
    mappend a      b      = Q [a, b]

class ToGpWord a where
    toGpWord :: a -> GpWord

instance ToGpWord Bool where
    toGpWord = B
instance ToGpWord Int where
    toGpWord = I
instance ToGpWord T.Text where
    toGpWord = S
instance ToGpWord a => ToGpWord [a] where
    toGpWord = Q . map toGpWord

instance Show GpWord where

    showsPrec _ (B True) s     = "#t" ++ s
    showsPrec _ (B False) s    = "#f" ++ s
    showsPrec _ (I i) s        = show i ++ s
    showsPrec _ (S t) s        = T.unpack t ++ s
    showsPrec n (Q qs) s = showq shows qs
        where
            showq _ []          = "[ ]" ++ s
            showq shows' (q:qs) = '[' : ' ' : shows' q (showl shows' qs)

            showl _  []     = ' ' : ']' : s
            showl sh (r:rs) = ' ' : sh r (showl sh rs)

data Stack = Stack [GpWord]
    deriving (Show)

instance Monoid Stack where
    mempty = Stack []
    mappend (Stack a) (Stack b) = Stack (a `mappend` b)

execq :: Quote -> Stack -> Stack
execq q s = foldl (flip exec) s q

exec :: GpWord -> Stack -> Stack
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

testWord :: GpWord
testWord = Q [I 42, B True, S "hi"]

testStack2 :: Stack
testStack2 = Stack [I 42]

testStack3 :: Stack
testStack3 = Stack [I 42, B True, S "hi"]

-- So this will compile
main :: IO ()
main = putStrLn "sigil"

