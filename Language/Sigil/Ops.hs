{-# LANGUAGE OverloadedStrings #-}

module Language.Sigil.Ops
    ( op
    ) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Monoid
import qualified Data.Text as T
import           Language.Sigil.Types

op :: T.Text -> StackTransformer

-- Stack operations

-- Boolean operations

-- Number operations

-- Integer operations

-- Double operations

-- Symbol operations

-- Vector operations

-- Quote operations

-- Finally, a no-op.
op _ s = return s

