{-# LANGUAGE TypeOperators #-}


module Language.Sigil.Stack
    ( swap
    , push
    , pop
    , nip
    , nip2
    , rotr
    , rotl
    ) where


import           Language.Sigil.Types


swap :: (s :. a :. b) -> (s :. b :. a)
swap (s :. a :. b) = s :. b :. a

push :: a -> s -> s :. a
push a s = s :. a

pop :: (s :. a) -> s
pop (s :. _) = s

nip :: (s :. a :. b) -> (s :. b)
nip (s :. _ :. b) = s :. b

nip2 :: (s :. a :. b :. c) -> (s :. c)
nip2 (s :. _ :. _ :. c) = s :. c

rotr :: (s :. a :. b :. c) -> (s :. c :. a :. b)
rotr (s :. a :. b :. c) = s :. c :. a :. b

rotl :: (s :. a :. b :. c) -> (s :. b :. c :. a)
rotl (s :. a :. b :. c) = s :. b :. c :. a

