-- See LICENSE for copyright and license details

{-# OPTIONS_GHC -Wall #-}

module Parser where

import Eval

import Control.Applicative
import Data.Char
import Data.Monoid

data ParserResult a = F String
                    | S (a, String)
                    deriving (Show) -- Fail or Success

instance Functor ParserResult where
  fmap _ (F err)     = F err
  fmap f (S (r, rs)) = S (f r, rs)
  {-# INLINE fmap #-}

newtype Parser a = P (String -> ParserResult a)

instance Functor Parser where
  fmap f p = P $ fmap f . parse p
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure a = P $ \xs -> S (a, xs)
  {-# INLINE pure #-}
  pf <*> pg = P $ \xs -> f <$> parse pf $ xs
    where
      f (F err)     = F err
      f (S (r, rs)) = parse (r <$> pg) rs
      {-# INLINE f #-}
  {-# INLINE (<*>) #-}

instance Alternative Parser where
  empty = P $ const $ F "empty"
  pf <|> pg = P $ \xs -> f xs <$> parse pf $ xs
    where
      f xs (F _)       = parse pg xs
      f _  (S (r, rs)) = S (r, rs)
      {-# INLINE f #-}
  {-# INLINE (<|>) #-}

parse :: Parser a -> String -> ParserResult a
parse (P a) = a

failP :: String -> Parser a
failP xs = P $ const $ F xs

satP :: (Char -> Bool) -> Parser Char
satP f = P g
  where
    g [] = F "expected input but got nothing"
    g (x:xs)
      | f x       = S (x, xs)
      | otherwise = F $ "unexpect character" <> show x

tokenP :: Parser a -> Parser a
tokenP p = pwspc *> p <* pwspc
  where
    pwspc = many $ satP isSpace

anyP :: Parser String
anyP = many (satP ('"' /=))

symbolP :: Parser String
symbolP = tokenP $ (:) <$> satP valid <*> many (satP valid)
  where
    valid' = getAny . foldMap (Any .) [isPunctuation, isSymbol, isAlphaNum]
    valid  = getAll . foldMap (All .) [valid', (/= '('), (/= ')')]
    -- NOTE: foldMap can be written as: foldr (mappend . (Any .)) mempty

matchErrP :: String -> Parser String
matchErrP xs = matchP xs <|> failP ("expected " <> xs)

matchP :: String -> Parser String
matchP [] = pure []
matchP (x:xs) = (:) <$> satP (== x) <*> matchP xs

optP :: Parser String -> Parser String
optP p = p <|> pure []

matchOptP :: String -> Parser String
matchOptP xs = optP $ matchP xs

numberP :: Parser Mexpr
numberP = tokenP (f <$> matchOptP "-" <*> digitP <*> optP ((<>) <$> matchP "." <*> digitP) <|> Mdub . read . ("0." <>) <$> (matchP "." *> digitP))
  where
    -- NOTE: i.e. 33. will be parsed as a 33 with "." left, this is intended.
    f s xs [] = Mnum $ read $ s <> xs
    f s xs ys = Mdub $ read $ s <> xs <> ys
    {-# INLINE f #-}

digitP :: Parser String
digitP = some $ satP isDigit

stringP :: Parser String
stringP = tokenP (matchErrP "\"" *> anyP) <* matchErrP "\""

nilP :: Parser Mexpr
nilP = tokenP (Mnil <$ matchP "nil")

-- TODO: move this to reader
listP :: Parser [Mexpr]
listP = tokenP (matchErrP "(" *> many exprP) <* matchErrP ")"

exprP :: Parser Mexpr
exprP = nilP <|> numberP <|> Mstr <$> stringP <|> Msym <$> symbolP <|> Mlst <$> listP
-- TODO: in here when reading reader macros we need to be able to parse and transform the syntax tree based on the
-- reader macro we are reading, leaving this here so i don't forget

metaP :: Parser Mexpr
metaP = exprP
