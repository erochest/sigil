{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import qualified Data.List as L
import qualified Data.Text as T


type Symbol = T.Text

-- | Types for values/words.
data GpQuote
data GpQ = forall a . ToGpWord a => GpQ a

data GpWord x where
    Nil :: GpWord ()
    B   :: Bool -> GpWord Bool
    I   :: Int -> GpWord Int
    S   :: Symbol -> GpWord Symbol
    Q   :: forall a . ToGpWord a => [a] -> GpWord GpQuote

class Show a => ToGpWord a where
    toGpWord :: forall b . a -> GpWord b

instance ToGpWord Bool where
    toGpWord = B
instance ToGpWord Int where
    toGpWord = I
instance ToGpWord T.Text where
    toGpWord = S
instance forall a . ToGpWord a => ToGpWord [a] where
    toGpWord = Q

instance Show (GpWord x) where
    showsPrec _ Nil s          = "#nil" ++ s
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

data Bottom
data Stack v r where
    SNil :: Stack () Bottom
    (:&) :: GpWord v -> s -> Stack v s
infixr 6 :&

{-
 - testWord :: GpWord x
 - testWord = Q [I 42, B True, S "hi"]
 - 
 - -- testStack1 :: Stack v r
 - testStack1 = SNil
 - 
 - -- testStack2 :: Stack v r
 - testStack2 = I 42 :& SNil
 - 
 - -- testStack3 :: Stack v r
 - testStack3 = I 42 :& B True :& S "hi" :& SNil
 -}

-- So this will compile
main :: IO ()
main = putStrLn "gplang"


