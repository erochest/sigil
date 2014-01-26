{-# LANGUAGE TypeOperators #-}


module Language.Sigil.Stack
    ( swap
    ) where


import           Language.Sigil.Types


swap :: (s :. a :. b) -> (s :. b :. a)
swap (s :. a :. b) = s :. b :. a


