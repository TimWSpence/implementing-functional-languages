module Parser.CombinatorParser
  ( Parser(..)
  , pParens
  , pLit
  , pSat
  , pVar
  , pNum
  , pZeroOrMore
  , pOneOrMore
  , pOneOrMoreWithSep
  ) where

import           Data.Char

import           Parser.Lexer

import           Control.Applicative

newtype Parser a = Parser { runParser :: [Token] -> [(a, [Token])] }

instance Functor Parser where
  fmap f p = Parser $ \toks -> do
    (v, toks2) <- runParser p toks
    return (f v, toks2)

instance Applicative Parser where
  pure a = Parser $ \toks -> [(a, toks)]

  f <*> p = Parser $ \toks -> do
    (fn, toks2) <- runParser f toks
    (v, toks3)  <- runParser p toks2
    return (fn v, toks3)

instance Alternative Parser where

  empty = Parser $ \_ -> []

  p1 <|> p2 = Parser $ \toks -> runParser p1 toks ++ runParser p2 toks

pLit :: String -> Parser String
pLit s = pSat (== s)

pVar :: Parser String
pVar = pSat $ \s -> s `notElem` keywords && isVar s
  where
    isVar []     = False
    isVar (c:cs) = isAlpha c && all isIdChar cs

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p <|> pure []

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = Parser $ \toks -> do
  (a, toks1) <- runParser p toks
  (rest, toks2) <- runParser (pZeroOrMore p) toks1
  return (a:rest, toks2)

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep pa pb = Parser $ \toks -> do
  (a, toks1) <- runParser pa toks
  (rest, toks2) <- runParser pab toks1
  return (a:rest, toks2)
    where
      pab = pZeroOrMore (liftA2 (\_ a -> a) pb pa)

pSat :: (String -> Bool) -> Parser String
pSat pred = Parser $ \t -> case t of
  ((_, tok):toks) | pred tok -> [(tok, toks)]
  _                          -> []

pNum :: Parser Int
pNum = read <$> (pSat (all isDigit))

pParens :: Parser a -> Parser a
pParens p = liftA3 (\_ v _ -> v) (pLit "(") p (pLit ")")
