{-# LANGUAGE ScopedTypeVariables #-}

module Language where

import           Data.List

type Name = String

data IsRec = Recursive | NotRecursive

isRec :: IsRec -> Bool
isRec Recursive    = True
isRec NotRecursive = False

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

data Expr a = EVar Name                         -- variables
            | ENum Int                          -- numbers
            | EConstr Int Int                   -- constructor tag arity
            | EAp (Expr a) (Expr a)             -- applications
            | ELet IsRec [(a, Expr a)] (Expr a) -- let expressions
            | ECase (Expr a) [Alter a]          -- case expressions
            | ELam [a] (Expr a)                 -- lambda abstractions

type CoreExpr = Expr Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False

bindersOf :: [(a,b)] -> [a]
bindersOf = fmap fst

rhssOf :: [(a,b)] -> [b]
rhssOf = fmap snd

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

prelude :: CoreProgram
prelude = [("I", ["x"], EVar "x")
  , ("K", ["x", "y"], EVar "x")
  , ("K1", ["x", "y"], EVar "y")
  , ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                             (EAp (EVar "g") (EVar "x")))
  , ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
  , ("twice", ["f"], EAp (EVar "f") (EVar "f"))
          ]


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

-- Allows formatting with efficient appends
data ISeq = INil
          | IStr String
          | IAppend ISeq ISeq
          deriving (Show)

instance Fmt ISeq where
  iNil = INil

  iStr = IStr

  iAppend = IAppend

  iNewline = iStr "\n"

  iIndent = id

  iDisplay seq = flatten [seq]

flatten :: [ISeq] -> String
flatten []                = ""
flatten (INil:tail)       = flatten tail
flatten (IStr str:tail) = str ++ flatten tail
flatten (IAppend seq1 seq2: tail) = flatten (seq1:seq2:tail)

pretty :: forall a. Fmt a => CoreExpr -> a
pretty (EVar x) = iStr x
pretty (ENum n) = iStr $ show n
pretty (EAp e1 e2) = pretty e1 `iAppend` iStr " " `iAppend` pretty e2
pretty (ELet irec defns expr) = iConcat [iStr keyword
                                        , iNewline
                                        , iStr " "
                                        , iIndent (prettyDefns defns)
                                        , iNewline
                                        , iStr "in"
                                        , pretty expr
                                        ]
  where
    keyword = if isRec irec then "letrec" else "let"

    prettyDefns :: [(Name, CoreExpr)] -> a
    prettyDefns defns = iInterleave sep (fmap prettyDefn defns)
      where
        sep :: a
        sep = iConcat [iStr ";", iNewline]

        prettyDefn :: (Name, CoreExpr) -> a
        prettyDefn (name, expr) = iConcat []
pretty _ = error "not implemented"
