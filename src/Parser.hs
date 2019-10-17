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
