module Parser.CombinatorParser
  ( Parser
  , pFail
  , pParens
  , pLit
  , pSat
  , pVar
  , pNum
  , pAlt
  , pThen
  , pThen3
  , pThen4
  , pZeroOrMore
  , pOneOrMore
  , pEmpty
  , pApply
  , pOneOrMoreWithSep
  ) where

import           Data.Char

import           Parser.Lexer

type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
pLit s = pSat (== s)

pVar :: Parser String
pVar = pSat $ \s -> s `notElem` keywords && isVar s
  where
    isVar []     = False
    isVar (c:cs) = isAlpha c && all isIdChar cs

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

-- TODO applicative instance for Parser
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen f p1 p2 toks = do
  (a, toks1) <- p1 toks
  (b, toks2) <- p2 toks1
  return (f a b, toks2)

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 f p1 p2 p3 toks = do
  (f2, toks1) <- pThen f p1 p2 toks
  (d, toks2) <- p3 toks1
  return (f2 d, toks2)

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 f p1 p2 p3 p4 toks = do
  (f2, toks1) <- pThen3 f p1 p2 p3 toks
  (e, toks2) <- p4 toks1
  return (f2 e, toks2)

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p toks = do
  (a, toks1) <- p toks
  (rest, toks2) <- pZeroOrMore p toks1
  return (a:rest, toks2)

pEmpty :: a -> Parser a
pEmpty a toks = [(a, toks)]

-- TODO functor instance for Parser
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = do
  (a, toks1) <- p toks
  return (f a, toks1)

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep pa pb toks = do
  (a, toks1) <- pa toks
  (rest, toks2) <- pab toks1
  return (a:rest, toks2)
    where
      pab = pZeroOrMore (pThen (\_ a -> a) pb pa)

pSat :: (String -> Bool) -> Parser String
pSat pred ((_, tok):toks) | pred tok = [(tok, toks)]
pSat _ _                  = []

pNum :: Parser Int
pNum = pApply (pSat (all isDigit)) read

pParens :: Parser a -> Parser a
pParens p = pThen3 (\_ v _ -> v) (pLit "(") p (pLit ")")

pFail :: Parser a
pFail _ = []
