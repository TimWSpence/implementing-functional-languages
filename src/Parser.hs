module Parser
  (
    parse
  ) where

import           AST
import           Control.Applicative
import           Parser.CombinatorParser
import           Parser.Lexer

parse :: String -> CoreProgram
parse = syntax . clex

syntax :: [Token] -> CoreProgram
syntax = take_first_parse . runParser pProgram
  where
    take_first_parse ((prog, []):_) = prog
    take_first_parse (_: others)    = take_first_parse others
    take_first_parse _              = error "Syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

liftA4 :: Applicative a => (v -> w -> x -> y -> z) -> a v -> a w -> a x -> a y -> a z
liftA4 f v w x y = f <$> v <*> w <*> x <*> y

pSc :: Parser CoreScDefn
pSc = liftA4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where
    mk_sc name vars _ expr = (name, vars, expr)

--TODO separate parsing of atomic expressions eg should only be able to apply to an atomic expr
pExpr :: Parser CoreExpr
pExpr = pELet <|> pECase <|> pELam <|> pExpr1
  where
    pELet = liftA4 (\_ binds _ expr -> ELet NotRecursive binds expr) (pLit "let") (pOneOrMoreWithSep pLet (pLit ";")) (pLit "in") pExpr
      <|> liftA4 (\_ binds _ expr -> ELet Recursive binds expr) (pLit "letrec") (pOneOrMoreWithSep pLet (pLit ";")) (pLit "in") pExpr
      where
        pLet = liftA3 (\var _ e -> (var, e)) pVar (pLit "=") pExpr

    pECase = liftA4 (\_ e _ cases -> ECase e cases) (pLit "case") pExpr (pLit "of") (pOneOrMoreWithSep pCase (pLit ";"))
      where
        pTag = liftA3 (\_ n _ -> n) (pLit "<") pNum (pLit ">")
        pCase = liftA4 (\t as _ e -> (t, as, e)) pTag (pZeroOrMore pVar) (pLit "->") pExpr

    pELam = liftA4 (\_ vars _ e -> ELam vars e) (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr

pExpr1 :: Parser CoreExpr
pExpr1 = pOr <|> pExpr2
  where
    pOr = liftA3 (\e _ e2 -> EBin (BOp Or) e e2) pExpr2 (pLit "||") pExpr1

pExpr2 :: Parser CoreExpr
pExpr2 = pAnd <|> pExpr3
  where
    pAnd = liftA3 (\e _ e2 -> EBin (BOp And) e e2) pExpr3 (pLit "&&") pExpr2

pExpr3 :: Parser CoreExpr
pExpr3 = pRel <|> pExpr4
  where
    pRel = pLT <|> pLE <|> pEQ <|> pNEQ <|> pGE <|> pGT
    pLT = liftA3 (\e _ e2 -> EBin (ROp RLT) e e2) pExpr4 (pLit "<") pExpr3 --TODO finish this mapping
    pLE = liftA3 (\e _ e2 -> EBin (ROp RLE) e e2) pExpr4 (pLit "<=") pExpr3 --TODO finish this mapping
    pEQ = liftA3 (\e _ e2 -> EBin (ROp REQ) e e2) pExpr4 (pLit "==") pExpr3 --TODO finish this mapping
    pNEQ = liftA3 (\e _ e2 -> EBin (ROp RNEQ) e e2) pExpr4 (pLit "~=") pExpr3 --TODO finish this mapping
    pGE = liftA3 (\e _ e2 -> EBin (ROp RGE) e e2) pExpr4 (pLit ">=") pExpr3 --TODO finish this mapping
    pGT = liftA3 (\e _ e2 -> EBin (ROp RGT) e e2) pExpr4 (pLit ">") pExpr3 --TODO finish this mapping

pExpr4 :: Parser CoreExpr
pExpr4 = pPlus <|> pMinus <|> pExpr5
  where
    pPlus = liftA3 (\e _ e2 -> EBin (NOp Plus) e e2) pExpr5 (pLit "+") pExpr4
    pMinus = liftA3 (\e _ e2 -> EBin (NOp Minus) e e2) pExpr5(pLit "-") pExpr4


pExpr5 :: Parser CoreExpr
pExpr5 = pTimes <|> pDiv <|> pExpr6
  where
    pTimes = liftA3 (\e _ e2 -> EBin (NOp Times) e e2) pExpr6 (pLit "*") pExpr5
    pDiv = liftA3 (\e _ e2 -> EBin (NOp Div) e e2) pExpr6 (pLit "/") pExpr5

pExpr6 :: Parser CoreExpr
pExpr6 = pAExpr <|> pEAp
  where
    pAExpr = pEVar <|> pENum <|> pEConstr <|> (pParens pExpr)

    pEVar = fmap EVar pVar

    pENum = fmap ENum pNum

    pEConstr = liftA4 (\_ _ (c, arity) _ -> EConstr c arity) (pLit "Pack") (pLit "{") pTags (pLit "}")
      where
        pTags = liftA3 (\c _ arity -> (c, arity)) pNum (pLit ",") pNum

    --A bit weird due to left recursion in the "obvious" implementation
    pEAp = fmap apChain (pOneOrMore pAExpr)
      where
        apChain [fn,arg] = EAp fn arg
        apChain _        = error "Syntax error in apply"
