{-# LANGUAGE TypeOperators #-}


module Language.Sigil.Stack
    ( push
    , pop
    ) where


import           Control.Monad.Free
import           Language.Sigil.Types


push :: a -> SigilProgram a ()
push x = liftF (Push x ())

pop :: SigilProgram a ()
pop = liftF (Pop ())

