
module Language.Sigil.Types
    ( Symbol
    , Quote
    , SWord(..)
    , Define(..)
    , DefMap
    , SigilProgram(..)
    , Stack(..)
    , Code(..)
    , StackTransformer
    , SigilEnv(..)
    , Sigil
    , SigilOutput
    ) where

import           Control.Monad.Trans.State
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Text as T
-- import qualified Data.Vector.Unboxed as V

type Symbol = T.Text

type Quote = [SWord]

data SWord
    = B  Bool
    | I  Int
    -- | F  Double
    | S  Symbol
    -- | VI (V.Vector Int)
    -- | VF (V.Vector Double)
    -- | VB (V.Vector Bool)
    | Q  Quote
    deriving (Eq)

instance Monoid SWord where
    mempty = Q []

    mappend (Q []) b      = b
    mappend a      (Q []) = a
    mappend (Q qa) (Q qb) = Q (qa `mappend` qb)
    mappend (Q q)  b      = Q (q ++ [b])
    mappend a      (Q q)  = Q (a:q)
    mappend a      b      = Q [a, b]

instance Show SWord where
    showsPrec _ (B True) s  = "#t" ++ s
    showsPrec _ (B False) s = "#f" ++ s
    showsPrec _ (I i) s     = show i ++ s
    -- showsPrec _ (F f) s     = show f ++ s
    showsPrec _ (S t) s     = T.unpack t ++ s
    -- showsPrec i (VI v) s    = showV 'i' i v s
    -- showsPrec i (VF v) s    = showV 'f' i v s
    -- showsPrec i (VB v) s    = showVB 'b' i v s
    showsPrec _ (Q qs) s    = showq shows qs
        where
            showq _ []          = "[ ]" ++ s
            showq shows' (x:xs) = '[' : ' ' : shows' x (showl shows' xs)

            showl _  []     = ' ' : ']' : s
            showl sh (r:rs) = ' ' : sh r (showl sh rs)

{-
 - showV :: (Show a, V.Unbox a) => Char -> Int -> V.Vector a -> ShowS
 - showV prefix _ v s = ('#':prefix:'<' : V.foldr accum (' ':'>' : s) v)
 -     where
 -         accum :: (Show a, V.Unbox a) => a -> String -> String
 -         accum i s = (' ' : show i ++ s)
 - 
 - showVB :: Char -> Int -> V.Vector Bool -> ShowS
 - showVB prefix _ v s = ('#':prefix:'<' : V.foldr accum ('>' : s) v)
 -     where
 -         accum :: Bool -> String -> String
 -         accum True  s = ('1' : s)
 -         accum False s = ('0' : s)
 -}

data Define = Define Symbol Quote
    deriving (Eq, Show)

type DefMap = M.Map Symbol Quote

data SigilProgram = Program { progDefines :: [Define]
                            , progCode    :: Quote
                            }
    deriving (Eq, Show)

instance Monoid SigilProgram where
    mempty        = Program mempty mempty
    mappend p1 p2 = Program (progDefines p1 `mappend` progDefines p2)
                            (progCode p1    `mappend` progCode p2)

data Stack = Stack [SWord]
    deriving (Show, Eq)

instance Monoid Stack where
    mempty = Stack []
    mappend (Stack a) (Stack b) = Stack (a `mappend` b)

type StackTransformer = Stack -> Sigil Stack

class Code a where

    -- This either generates a stack transformation function or it executes an
    -- instruction.
    exec :: a -> StackTransformer

data SigilEnv = SigilEnv
    { envProgram :: Maybe SigilProgram
    , envDefines :: DefMap
    } deriving (Show)

type Sigil = StateT SigilEnv IO

type SigilOutput = (Stack, SigilEnv)

