{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}


module Language.Sigil.Types
    ( (:.)(..)
    , (:>)
    , Sigil(..)
    , SigilProgram
    ) where


import           Data.Functor

import           Control.Monad.Free


infixl 1 :.
data s :. a = !s :. !a
            deriving (Show, Eq)

instance Functor ((:.) s) where
    fmap f (s :. a) = s :. f a

infixl 2 :>
type s :> s' = s -> s'

data Sigil s next
    = Push s next
    | Pop next
    deriving (Functor)

type SigilProgram a = Free (Sigil a)

