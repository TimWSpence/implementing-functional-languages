{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language where

import Format
import Parser
import Data.List

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

instance Show CoreExpr where
  show e = iDisplay . pretty @ISeq $ e

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

instance {-# Overlapping #-} Show CoreScDefn where
  show (name, vars, e) = name <> (if vars == [] then "" else (" " <> intercalate "," vars)) <> " = " <> show e

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
pretty (EAp e1 e2) = pretty e1 `iAppend` iStr " " `iAppend` pretty e2
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
pretty _ = error "not implemented"

parse :: String -> CoreProgram
parse = syntax . clex

syntax :: [Token] -> CoreProgram
syntax = take_first_parse . pProgram
  where
    take_first_parse ((prog, []):_) = prog
    take_first_parse (_: others) = take_first_parse others
    take_first_parse _ = error "Syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where
    mk_sc name vars _ expr = (name, vars, expr)

pExpr :: Parser CoreExpr
pExpr = pEVar `pAlt` pENum `pAlt` pEConstr `pAlt` pELet `pAlt` pECase `pAlt` pELam
  where
    pEVar = pApply pVar EVar
    pENum = pApply pNum ENum
    pEConstr = pThen4 (\_ _ (c, arity) _ -> EConstr c arity) (pLit "Pack") (pLit "{") pTags (pLit "}")
      where
        pTags = pThen3 (\c _ arity -> (c, arity)) pNum (pLit ",") pNum
    pEAp = undefined
    pELet = pThen4 (\_ binds _ expr -> ELet NotRecursive binds expr) (pLit "let") (pOneOrMore pLet) (pLit "in") pExpr
      `pAlt` pThen4 (\_ binds _ expr -> ELet Recursive binds expr) (pLit "letrec") (pOneOrMore pLet) (pLit "in") pExpr
      where
        pLet = pThen4 (\var _ e _ -> (var, e))   pVar (pLit "=") pExpr (pLit ";")
    pECase = pThen4 (\_ e _ cases -> ECase e cases) (pLit "case") pExpr (pLit "of") (pOneOrMore pCase)
      where
        pTag = pThen3 (\_ n _ -> n) (pLit "<") pNum (pLit ">")
        pCase = pThen4 (\t as _ e -> (t, as, e)) pTag (pZeroOrMore pVar) (pLit "->") pExpr
    pELam = pParens $ pThen4 (\_ vars _ e -> ELam vars e) (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr

-- data Expr a = EVar Name                         -- variables
--             | ENum Int                          -- numbers
--             | EConstr Int Int                   -- constructor tag arity

--             | EAp (Expr a) (Expr a)             -- applications
--             | ELet IsRec [(a, Expr a)] (Expr a) -- let expressions
--             | ECase (Expr a) [Alter a]          -- case expressions
--             | ELam [a] (Expr a)                 -- lambda abstractions
