module Parser
  (
  ) where

import           Data.Char


type Token = String

clex :: String -> [Token]
clex [] = []
clex (x:y:t) | [x,y] `elem` twoCharOps = [x,y] : clex t
clex (c:cs)
  | isSpace c = clex cs
  | isDigit c = num_token : clex num_rest
  | isAlpha c = var_tok : clex var_rest
  where
    num_token = c : takeWhile isDigit cs
    num_rest = dropWhile isDigit cs
    var_tok = c : takeWhile isIdChar cs
    var_rest = dropWhile isIdChar cs

    isIdChar c = isAlpha c || isDigit c || (c == '_')

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]
