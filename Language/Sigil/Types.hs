
module Language.Sigil.Types
    ( Symbol
    , Quote
    , SWord(..)
    ) where

import           Data.Monoid
import qualified Data.Text as T

type Symbol = T.Text

type Quote = [SWord]

data SWord
    = B Bool
    | I Int
    | S Symbol
    | Q Quote
    deriving (Eq)

instance Monoid SWord where
    mempty = Q []

    mappend (Q []) b      = b
    mappend a      (Q []) = a
    mappend (Q qa) (Q qb) = Q (qa `mappend` qb)
    mappend (Q q)  b      = Q (q ++ [b])
    mappend a      (Q q)  = Q (a:q)
    mappend a      b      = Q [a, b]

instance Show SWord where
    showsPrec _ (B True) s  = "#t" ++ s
    showsPrec _ (B False) s = "#f" ++ s
    showsPrec _ (I i) s     = show i ++ s
    showsPrec _ (S t) s     = T.unpack t ++ s
    showsPrec _ (Q qs) s    = showq shows qs
        where
            showq _ []          = "[ ]" ++ s
            showq shows' (x:xs) = '[' : ' ' : shows' x (showl shows' xs)

            showl _  []     = ' ' : ']' : s
            showl sh (r:rs) = ' ' : sh r (showl sh rs)


