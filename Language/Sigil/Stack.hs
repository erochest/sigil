{-# LANGUAGE TypeOperators #-}


module Language.Sigil.Stack
    ( swap
    , swapd
    , push
    , pop
    , nip
    , nip2
    , rotr
    , rotl
    , dup
    , dup2
    , dupd
    ) where


import           Language.Sigil.Types


swap :: (s :. a :. b) -> (s :. b :. a)
swap (s :. a :. b) = s :. b :. a

swapd :: s :. a :. b :. c -> s :. b :. a :. c
swapd (s :. a :. b :. c) = s :. b :. a :. c

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

dup   :: s :. a -> s :. a :. a
dup s@(_ :. a) = s :. a

dup2  :: s :. a :. b -> s :. a :. b :. a :. b
dup2 s@(_ :. a :. b) = s :. a :. b

dupd  :: s :. a :. b -> s :. a :. a :. b
dupd (s :. a :. b) = s :. a :. a :. b
