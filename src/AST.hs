{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AST
  (
    Expr(..)
  , BinOp(..)
  , NumOp(..)
  , BoolOp(..)
  , RelOp(..)
  , Name
  , IsRec(..)
  , Alter
  , CoreAlt
  , Program
  , CoreProgram
  , ScDefn
  , CoreScDefn
  , CoreExpr
  , prelude
  , isRec
  , isAtomicExpr
  , bindersOf
  , rhssOf
  ) where

import           Data.List
import           Format

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
            | EBin BinOp (Expr a) (Expr a)

data BinOp = NOp NumOp | BOp BoolOp | ROp RelOp
data NumOp = Plus | Minus | Times | Div
data BoolOp = And | Or
data RelOp = RLT | RLE | REQ | RNEQ | RGE | RGT

instance Show NumOp where
  show Plus  = "+"
  show Minus = "-"
  show Times = "*"
  show Div   = "/"

instance Show BoolOp where
  show And = "&&"
  show Or  = "||"

instance Show RelOp where
  show RLT  = "<"
  show RLE  = "<="
  show REQ  = "=="
  show RNEQ = "~="
  show RGE  = ">="
  show RGT  = ">"

instance Show BinOp where
  show (NOp op) = show op
  show (BOp op) = show op
  show (ROp op) = show op

instance Show CoreExpr where
  show = iDisplay . pretty @ISeq

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

instance {-# Overlapping #-} Show CoreProgram where
  show = intercalate ";\n" . fmap (show @CoreScDefn)

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

instance {-# Overlapping #-} Show CoreScDefn where
  show (name, vars, e) = name <> (if vars == [] then "" else (" " <> intercalate " " vars)) <> " = " <> show e

prelude :: CoreProgram
prelude = [("I", ["x"], EVar "x")
  , ("K", ["x", "y"], EVar "x")
  , ("K1", ["x", "y"], EVar "y")
  , ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                             (EAp (EVar "g") (EVar "x")))
  , ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
  , ("twice", ["f"], EAp (EVar "f") (EVar "f"))
          ]

pretty :: forall a. Fmt a => CoreExpr -> a
pretty (EVar x) = iStr x
pretty (ENum n) = iStr $ show n
pretty (EAp e1 e2) = pretty e1 `iAppend` iStr " (" `iAppend` pretty e2 `iAppend` iStr ")"
pretty (ELet irec defns expr) = iConcat [iStr keyword
                                        , iNewline
                                        , iStr "  "
                                        , iIndent (prettyDefns defns)
                                        , iNewline
                                        , iStr "in "
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
        prettyDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent . pretty $ expr]
pretty (ECase e as) = iConcat [iStr "case "
                              , pretty e
                              , iStr " of "
                              , iIndent (prettyAlters as)
                              ]
  where
    prettyAlters :: [Alter Name] -> a
    prettyAlters as = iInterleave sep (fmap prettyAlter as)

    prettyAlter (tag, vars, e) = iConcat [iStr "<"
                                         , iStr $ show tag
                                         , iStr "> "
                                         , iInterleave (iStr ",") (fmap iStr vars)
                                         , iStr " -> "
                                         , pretty e
                                         ]

    sep = iConcat [iStr ";", iNewline]
pretty (ELam vars e) = iConcat [iStr "\\"
                               , iInterleave (iStr " ") (fmap iStr vars)
                               , iStr ". "
                               , pretty e
                               ]
pretty (EBin op e1 e2) = iConcat [pretty e1
                                 , iStr (" " <> show op <> " ")
                                 , pretty e2]
pretty _ = error "not implemented"

