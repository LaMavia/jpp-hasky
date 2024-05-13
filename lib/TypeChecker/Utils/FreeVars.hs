{-# LANGUAGE OverloadedStrings #-}

module TypeChecker.Utils.FreeVars where

import qualified Abs
import           Common                      (unions)
import           Data.List                   (singleton, (\\))
import           Parser
import           TypeChecker.TC              (Type (TCAny, TCApp, TCBound, TCData, TCVar))
import           TypeChecker.Utils.BoundVars (bvOfTopDef, bvOfType,
                                              stringOfLident)

type FV a = a -> [String]


fvOfLIdent :: FV Abs.LIdent
fvOfLIdent _ = []

fvOfUIdent :: FV Abs.UIdent
fvOfUIdent _ = []

-- |
-- >>>  fvOfProgram ("type List(a) = Cons(c, a, b) ;;" :: Abs.Program)
-- ["c","b"]

-- |
-- >>> fvOfProgram ("f = fun (x b, y a) -> _ ;;" :: Abs.Program)
-- ["b","a"]
--
fvOfProgram :: (Show a) => FV (Abs.Program' a)
fvOfProgram x = case x of
  Abs.Program _ topdefs -> unions $ fvOfTopDef <$> topdefs

fvOfTopDef :: (Show a) => FV (Abs.TopDef' a)
fvOfTopDef x = case x of
  Abs.TDDataV _ _ args constructors ->
    unions (fvOfConstructor <$> constructors) \\ unions (singleton . stringOfLident <$> args)
  Abs.TDDataNV _ _ constructors ->
    unions $ fvOfConstructor <$> constructors
  Abs.TDDeclaration _ _ t expr ->
    fvOfExpr expr \\ bvOfType t
  Abs.TDDeclarationNT _ _ expr ->
    fvOfExpr expr

fvOfType :: (Show a) => FV (Abs.Type' a)
fvOfType x = case x of
  Abs.TVar _ (Abs.LIdent v) -> [v]
  Abs.TApp _ _ types ->
    unions $ fvOfType <$> types
  Abs.TType {} -> []
  Abs.TBound _ _ type_ -> fvOfType type_

fvOfConstructor :: (Show a) => FV (Abs.Constructor' a)
fvOfConstructor x = case x of
  Abs.Constructor _ _ types -> unions $ fvOfType <$> types
  Abs.NullaryConstr {}      -> []

fvOfArg :: (Show a) => FV (Abs.Arg' a)
fvOfArg x = case x of
  Abs.Arg _ _ type_ -> fvOfType type_

fvOfExpr :: (Show a) => FV (Abs.Expr' a)
fvOfExpr x = case x of
  Abs.ELet _ expr1 type_ expr2 expr3 ->
    unions
      [ fvOfExpr expr1,
        fvOfExpr expr2,
        fvOfExpr expr3,
        fvOfType type_
      ]
  Abs.ELetNT _ expr1 expr2 expr3 ->
     unions
      [ fvOfExpr expr1,
        fvOfExpr expr2,
        fvOfExpr expr3
      ]
  Abs.EMatch _ expr matchbranchs ->
    unions (fvOfExpr expr : (fvOfMatchBranch <$> matchbranchs))
  Abs.EIf _ expr1 expr2 expr3 ->
    unions
      [ fvOfExpr expr1,
        fvOfExpr expr2,
        fvOfExpr expr3
      ]
  Abs.ELambda _ args expr ->
    unions (fvOfExpr expr : (fvOfArg <$> args))
  Abs.EList _ exprs -> unions (fvOfExpr <$> exprs)
  Abs.EId {} -> []
  Abs.EConstr {} -> []
  Abs.EIgnore {} -> []
  Abs.EApp _ expr exprs ->
    unions (fvOfExpr expr : (fvOfExpr <$> exprs))
  Abs.ELit {} -> []
  Abs.Neg _ expr -> fvOfExpr expr
  Abs.Not _ expr -> fvOfExpr expr
  Abs.EMul _ expr1 mulop expr2 -> unions [fvOfExpr expr1, fvOfExpr expr2, fvOfMulOp mulop]
  Abs.EAdd _ expr1 addop expr2 -> unions [fvOfExpr expr1, fvOfExpr expr2, fvOfAddOp addop]
  Abs.ERel _ expr1 relop expr2 -> unions [fvOfExpr expr1, fvOfExpr expr2, fvOfRelOp relop]
  Abs.EAnd _ expr1 expr2 -> unions (fvOfExpr <$> [expr1, expr2])
  Abs.EOr _ expr1 expr2 -> unions (fvOfExpr <$> [expr1, expr2])

fvOfMatchBranch :: (Show a) => FV (Abs.MatchBranch' a)
fvOfMatchBranch _ = []

fvOfLiteral :: (Show a) => FV (Abs.Literal' a)
fvOfLiteral _ = []

fvOfAddOp :: (Show a) => FV (Abs.AddOp' a)
fvOfAddOp _ = []

fvOfMulOp :: (Show a) => FV (Abs.MulOp' a)
fvOfMulOp _ = []

fvOfRelOp :: (Show a) => FV (Abs.RelOp' a)
fvOfRelOp _ = []

tcFVOfType :: FV Type
tcFVOfType x = case x of
  TCVar v      -> [v]
  TCAny        -> []
  TCApp _ ts   -> unions $ tcFVOfType <$> ts
  TCBound vs t -> tcFVOfType t \\ vs
  TCData {}    -> []
