{-# LANGUAGE TypeOperators #-}


module Language.Sigil.Stack
    ( swap
    , push
    , pop
    ) where


import           Language.Sigil.Types


swap :: (s :. a :. b) -> (s :. b :. a)
swap (s :. a :. b) = s :. b :. a

push :: a -> s -> s :. a
push a s = s :. a

pop :: (s :. a) -> s
pop (s :. _) = s

