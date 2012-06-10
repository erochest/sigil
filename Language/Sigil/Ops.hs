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
op "and" (Stack (B a:B b:ss)) = return $ Stack (B (a && b):ss)
op "or"  (Stack (B a:B b:ss)) = return $ Stack (B (a || b):ss)
op "not" (Stack (B a:ss))     = return $ Stack (B (not a) :ss)

-- Integer operations
op "add_int" (Stack (I a:I b:ss)) = return $ Stack (I (a + b):ss)

-- Double operations

-- Symbol operations

-- Vector operations

-- Quote operations

-- Finally, a no-op.
op _ s = return s

