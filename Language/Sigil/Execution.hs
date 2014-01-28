{-# LANGUAGE TypeOperators #-}


module Language.Sigil.Execution
    ( apply
    , dip
    , dip2
    , dip3
    , dip4
    ) where


import           Language.Sigil.Types


apply :: (s :. s :> s') -> s'
apply (s :. f) = f s

dip :: s :. (s :> s') :. x -> s' :. x
dip (s :. f :. a) = f s :. a

dip2 :: s :. (s :> s') :. x :. y -> s' :. x :. y
dip2 (s :. f :. a :. b) = f s :. a :. b

dip3 :: s :. (s :> s') :. x :. y :. z -> s' :. x :. y :. z
dip3 (s :. f :. a :. b :. c) = f s :. a :. b :. c

dip4 :: s :. (s :> s') :. x :. y :. z :. a -> s' :. x :. y :. z :. a
dip4 (s :. f :. a :. b :. c :. d) = f s :. a :. b :. c :. d

