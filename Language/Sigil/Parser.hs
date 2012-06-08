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
import qualified Data.Vector.Unboxed as V
import           Language.Sigil.Types
import           Prelude hiding (takeWhile)


parseText :: T.Text -> Either String SWord
parseText = parseOnly word

word :: Parser SWord
word =   quote
     <|> vector
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

vector :: Parser SWord
vector =   vector' "#f<" tof VF
       <|> vector' "#i<" toi VI
       <|> vectorbool

vector' :: (V.Unbox a)
        => T.Text
        -> (Number -> Parser a)
        -> (V.Vector a -> SWord)
        -> Parser SWord
vector' prefix pnumber toWord = try $
    (stringCI prefix >> skipSpace) *> vbody <* (skipSpace >> char '>')
    where
        vbody :: Parser SWord
        vbody = toWord . V.fromList <$> (pnumber =<< number) `sepBy` many1 space

vectorbool :: Parser SWord
vectorbool = try $
    (stringCI "#b<" >> skipSpace) *> vbody <* (skipSpace >> char '>')
    where
        vbody :: Parser SWord
        vbody = VB . V.fromList <$> many (satisfy isBoolDigit >>= tob)

tob :: Char -> Parser Bool
tob '0' = return False
tob ' ' = return False
tob _   = return True

isBoolDigit :: Char -> Bool
isBoolDigit '0' = True
isBoolDigit '1' = True
isBoolDigit _   = False

float :: Parser SWord
float = F <$> (tof =<< number)

tof :: Number -> Parser Double
tof (P.D f) = return f
tof _       = fail "not a floating-point number"

int :: Parser SWord
int = I <$> (toi =<< number)

toi :: Number -> Parser Int
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

