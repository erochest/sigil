{-# LANGUAGE TypeOperators #-}


module Language.Sigil.Types
    ( (:.)(..)
    ) where


import           Data.Functor ()


infixl 1 :.
data s :. a = !s :. !a
            deriving (Show, Eq)

instance Functor ((:.) s) where
    fmap f (s :. a) = s :. f a

