
module Language.Sigil.Parser
    ( parseText
    ) where

import qualified Data.Text as T
import           Language.Sigil.Types

parseText :: T.Text -> Either String SWord
parseText _ = Left "noop"

