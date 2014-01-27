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
    , over
    , over2
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

over  :: s :. a :. b -> s :. a :. b :. a
over s@(_ :. a :. _) = s :. a

over2 :: s :. a :. b :. c -> s :. a :. b :. c :. a :. b
over2 s@(_ :. a :. b :. _) = s :. a :. b
