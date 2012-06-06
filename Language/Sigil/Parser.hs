{-# LANGUAGE OverloadedStrings #-}

module Language.Sigil.Parser
    ( parseText
    ) where

import           Control.Applicative
import qualified Data.Attoparsec.Text as P
import           Data.Attoparsec.Text hiding (I)
import           Data.Char
import           Data.Either
import qualified Data.Text as T
import           Language.Sigil.Types
import           Prelude hiding (takeWhile)


parseText :: T.Text -> Either String SWord
parseText = parseOnly word

word :: Parser SWord
word =   quote
     <|> bool
     <|> float
     <|> int
     <|> symbol

quote :: Parser SWord
quote = do
    _ <- char '['
    skipSpace
    qs <- word `sepBy` (takeWhile1 isSpace)
    skipSpace
    _ <- char ']'
    return $ Q qs

bool :: Parser SWord
bool =   (stringCI "#t" >> return (B True))
     <|> (stringCI "#f" >> return (B False))

float :: Parser SWord
float = F <$> (tof =<< number)
    where
        tof (P.D f) = return f
        tof _       = fail "not a floating-point number"

int :: Parser SWord
int = I <$> (toi =<< number)
    where
        toi (P.I i) = return $ fromIntegral i
        toi _       = fail "not an integer"

symbol :: Parser SWord
symbol = do
    c  <- satisfy $ \c -> isSymbolChar c && not (isDigit c)
    cs <- takeWhile isSymbolChar
    return . S . T.toLower $ T.cons c cs
    where
        isSymbolChar '[' = False
        isSymbolChar ']' = False
        isSymbolChar c   = not $ isSpace c

