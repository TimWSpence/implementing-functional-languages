module Parser
  (
  ) where

import           Data.Char


type LineNumber = Int
type Token = (LineNumber, String)

clex :: String -> [Token]
clex = _clex 0
  where
  _clex :: Int -> String -> [Token]
  _clex _ [] = []
  _clex n (x:y:t) | [x,y] `elem` twoCharOps = (n, [x,y]) : _clex n t
  _clex n (c:cs)
    | c == '\n' = _clex (n+1) cs
    | isSpace c = _clex n cs
    | isDigit c = (n, num_token) : _clex n num_rest
    | isAlpha c = (n, var_tok) : _clex n var_rest
    where
      num_token = c : takeWhile isDigit cs
      num_rest = dropWhile isDigit cs
      var_tok = c : takeWhile isIdChar cs
      var_rest = dropWhile isIdChar cs

      isIdChar c = isAlpha c || isDigit c || (c == '_')

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
pLit s ((_, tok):toks) | s == tok = [(s, toks)]
pLit _ _ = []

pVar :: Parser String
pVar _ = [] --TODO

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen f p1 p2 toks = do
  (a, toks1) <- p1 toks
  (b, toks2) <- p2 toks1
  return (f a b, toks2)
