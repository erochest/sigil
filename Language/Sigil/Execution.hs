{-# LANGUAGE TypeOperators #-}


module Language.Sigil.Execution
    ( apply
    ) where


import           Language.Sigil.Types


apply :: (s :. SigilFn s s') -> s'
apply (s :. f) = f s

