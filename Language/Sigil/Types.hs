{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}


module Language.Sigil.Types
    ( (:.)(..)
    , (:-)(..)
    , (:>)
    , Sigil(..)
    , SigilProgram
    ) where


import           Control.Arrow
import           Control.Monad.Free


infixl 1 :.
data s :. a = !s :. !a
            deriving (Show, Eq)

instance Functor ((:.) s) where
    fmap f (s :. a) = s :. f a

infixl 2 :>
type s :> s' = s -> s'

infixl 3 :-
data s :- s' = !s :- !s'
              deriving (Show, Eq)

data Sigil a next
    = Push a next
    | Pop next
    deriving (Functor)

type SigilProgram a = Free (Sigil a)

