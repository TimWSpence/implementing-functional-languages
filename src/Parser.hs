module Parser
  (
    parse
  ) where

import           AST
import           Parser.CombinatorParser
import           Parser.Lexer

parse :: String -> CoreProgram
parse = syntax . clex

syntax :: [Token] -> CoreProgram
syntax = take_first_parse . pProgram
  where
    take_first_parse ((prog, []):_) = prog
    take_first_parse (_: others)    = take_first_parse others
    take_first_parse _              = error "Syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where
    mk_sc name vars _ expr = (name, vars, expr)

--TODO separate parsing of atomic expressions eg should only be able to apply to an atomic expr
pExpr :: Parser CoreExpr
pExpr = pELet `pAlt` pECase `pAlt` pELam `pAlt` pExpr1
  where
    pELet = pThen4 (\_ binds _ expr -> ELet NotRecursive binds expr) (pLit "let") (pOneOrMoreWithSep pLet (pLit ";")) (pLit "in") pExpr
      `pAlt` pThen4 (\_ binds _ expr -> ELet Recursive binds expr) (pLit "letrec") (pOneOrMoreWithSep pLet (pLit ";")) (pLit "in") pExpr
      where
        pLet = pThen3 (\var _ e -> (var, e)) pVar (pLit "=") pExpr

    pECase = pThen4 (\_ e _ cases -> ECase e cases) (pLit "case") pExpr (pLit "of") (pOneOrMoreWithSep pCase (pLit ";"))
      where
        pTag = pThen3 (\_ n _ -> n) (pLit "<") pNum (pLit ">")
        pCase = pThen4 (\t as _ e -> (t, as, e)) pTag (pZeroOrMore pVar) (pLit "->") pExpr

    pELam = pThen4 (\_ vars _ e -> ELam vars e) (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr

pExpr1 :: Parser CoreExpr
pExpr1 = pOr `pAlt` pExpr2
  where
    pOr = pThen3 (\e _ e2 -> EBin (BOp Or) e e2) pExpr2 (pLit "||") pExpr1

pExpr2 :: Parser CoreExpr
pExpr2 = pAnd `pAlt` pExpr3
  where
    pAnd = pThen3 (\e _ e2 -> EBin (BOp And) e e2) pExpr3 (pLit "&&") pExpr2

pExpr3 :: Parser CoreExpr
pExpr3 = pRel `pAlt` pExpr4
  where
    pRel = pLT `pAlt` pLE `pAlt` pEQ `pAlt` pNEQ `pAlt` pGE `pAlt` pGT
    pLT = pThen3 (\e _ e2 -> EBin (ROp RLT) e e2) pExpr4 (pLit "<") pExpr3 --TODO finish this mapping
    pLE = pThen3 (\e _ e2 -> EBin (ROp RLE) e e2) pExpr4 (pLit "<=") pExpr3 --TODO finish this mapping
    pEQ = pThen3 (\e _ e2 -> EBin (ROp REQ) e e2) pExpr4 (pLit "==") pExpr3 --TODO finish this mapping
    pNEQ = pThen3 (\e _ e2 -> EBin (ROp RNEQ) e e2) pExpr4 (pLit "~=") pExpr3 --TODO finish this mapping
    pGE = pThen3 (\e _ e2 -> EBin (ROp RGE) e e2) pExpr4 (pLit ">=") pExpr3 --TODO finish this mapping
    pGT = pThen3 (\e _ e2 -> EBin (ROp RGT) e e2) pExpr4 (pLit ">") pExpr3 --TODO finish this mapping

pExpr4 :: Parser CoreExpr
pExpr4 = pPlus `pAlt` pMinus `pAlt` pExpr5
  where
    pPlus = pThen3 (\e _ e2 -> EBin (NOp Plus) e e2) pExpr5 (pLit "+") pExpr4
    pMinus = pThen3 (\e _ e2 -> EBin (NOp Minus) e e2) pExpr5(pLit "-") pExpr4


pExpr5 :: Parser CoreExpr
pExpr5 = pTimes `pAlt` pDiv `pAlt` pExpr6
  where
    pTimes = pThen3 (\e _ e2 -> EBin (NOp Times) e e2) pExpr6 (pLit "*") pExpr5
    pDiv = pThen3 (\e _ e2 -> EBin (NOp Div) e e2) pExpr6 (pLit "/") pExpr5

pExpr6 :: Parser CoreExpr
pExpr6 = pAExpr `pAlt` pEAp
  where
    pAExpr = pEVar `pAlt` pENum `pAlt` pEConstr `pAlt` (pParens pExpr)

    pEVar = pApply pVar EVar

    pENum = pApply pNum ENum

    pEConstr = pThen4 (\_ _ (c, arity) _ -> EConstr c arity) (pLit "Pack") (pLit "{") pTags (pLit "}")
      where
        pTags = pThen3 (\c _ arity -> (c, arity)) pNum (pLit ",") pNum

    --A bit weird due to left recursion in the "obvious" implementation
    pEAp = pApply (pOneOrMore pAExpr) apChain
      where
        apChain [fn,arg] = EAp fn arg
        apChain _        = error "Syntax error in apply"
