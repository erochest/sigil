{-# LANGUAGE OverloadedStrings #-}


module Language.Sigil.Show
    ( showSigil
    ) where


import           Control.Monad.Free

import           Language.Sigil.Types


showSigil :: (Show a, Show r) => Free (Sigil a) r -> String
showSigil (Free (Push s next)) = show s ++ "\n" ++ showSigil next
showSigil (Free (Pop next))    = ".\n" ++ showSigil next
showSigil (Pure r)             = show r ++ "\n"

