{-# LANGUAGE OverloadedStrings #-}

module Language.Sigil.Ops
    ( op
    ) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Monoid
import qualified Data.Text as T
import           Language.Sigil.Types

op :: T.Text -> Stack -> Sigil Stack
op _ s = return s

