{-# LANGUAGE TypeOperators #-}


module Language.Sigil.Execution
    ( apply
    , dip
    , dip2
    , dip3
    , dip4
    , keep
    , keep2
    , keep3
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

keep  :: s :. x :. ((s :. x) :> s') -> s' :. x
keep (s@(_ :. x) :. f) = f s :. x

keep2 :: s :. x :. y :. ((s :. x :. y) :> s') -> s' :. x :. y
keep2 (s@(_ :. x :. y) :. f) = f s :. x :. y

keep3 :: s :. x :. y :. z :. ((s :. x :. y :. z) :> s') -> s' :. x :. y :. z
keep3 (s@(_ :. x :. y :. z) :. f) = f s :. x :. y :. z

