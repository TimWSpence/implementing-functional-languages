module Format (
  Fmt(..)
, ISeq
, iConcat
, iInterleave
, iNum
, iFWNum
, iLayn
              ) where

import           Data.List

class Fmt a where
  iNil :: a

  iStr :: String -> a

  iAppend :: a -> a -> a

  iNewline :: a

  iIndent :: a -> a

  iDisplay :: a -> String

iConcat :: Fmt a => [a] -> a
iConcat = foldl' iAppend iNil

iInterleave :: Fmt a => a -> [a] -> a
iInterleave delim = iConcat . intersperse delim

iNum :: Fmt a => Int -> a
iNum = iStr . show

iFWNum :: Fmt a => Int -> Int -> a
iFWNum width n = iStr (replicate m ' ' ++ s)
  where
    s = show n
    m = width - length s

iLayn :: Fmt a => [a] -> a
iLayn xs = iConcat (fmap layItem $ zip [1..] xs)
  where
    layItem (n, x) = iConcat [iFWNum 4 n, iStr ") ", iIndent x, iNewline]

-- Allows formatting with efficient appends
data ISeq = INil
          | IStr String
          | IAppend ISeq ISeq
          | IIndent ISeq
          | INewline
          deriving (Show)

instance Fmt ISeq where
  iNil = INil

  iStr = IStr

  iAppend = IAppend

  iNewline = INewline

  iIndent = IIndent

  iDisplay s = flatten 0 [(s,0)]

flatten :: Int -> [(ISeq, Int)] -> String
flatten _ []                             = ""
flatten col ((INil, _):t)              = flatten col t
flatten col ((IStr str, _):t)          = str ++ flatten (col + length str) t
flatten col ((IAppend seq1 seq2, ind):t) = flatten col ((seq1, ind):(seq2, ind):t)
flatten _ ((INewline, ind):t)            = '\n' : replicate ind ' ' ++ flatten ind t
flatten col ((IIndent seq1, _):t)      = flatten col ((seq1, col) : t)
