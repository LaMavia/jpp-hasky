module TypeChecker.Utils.FreeVars where

import qualified Abs
import qualified Data.Set                    as Set
import           TypeChecker.TC              (Type (TCAny, TCApp, TCBound, TCData, TCInt, TCVar))
import           TypeChecker.Utils.BoundVars (bvOfTopDef, bvOfType)

type FV a = Set.Set String -> a -> Set.Set String

fvOfLIdent :: FV Abs.LIdent
fvOfLIdent _ = const Set.empty

fvOfUIdent :: FV Abs.UIdent
fvOfUIdent _ = const Set.empty

-- data List (a) = Cons a b
--
-- >>>  fvOfProgram Set.empty ( Abs.Program Nothing [ Abs.TDDataV Nothing (Abs.UIdent "List") [Abs.LIdent "a"] [ Abs.Constructor Nothing (Abs.UIdent "Cons") [ Abs.TVar Nothing (Abs.LIdent "a") , Abs.TVar Nothing (Abs.LIdent "b") ] ] ])
-- fromList ["b"]

-- f = fun (x :: a, y :: b) -> _
--
-- >>> fvOfTopDef Set.empty ( Abs.TDDeclarationNT Nothing (Abs.LIdent "f") (Abs.ELambda Nothing [ Abs.Arg Nothing (Abs.LIdent "x") (Abs.TVar Nothing (Abs.LIdent "a")) , Abs.Arg Nothing (Abs.LIdent "y") (Abs.TVar Nothing (Abs.LIdent "b")) ] (Abs.EIgnore Nothing) ))
-- fromList ["a","b"]
fvOfProgram :: (Show a) => FV (Abs.Program' a)
fvOfProgram s x = case x of
  Abs.Program _ topdefs -> Set.unions (fvOfTopDef s <$> topdefs) `Set.difference` s

fvOfTopDef :: (Show a) => FV (Abs.TopDef' a)
fvOfTopDef s x = case x of
  Abs.TDDataV _ _ _ constructors ->
    Set.unions (fvOfConstructor s' <$> constructors) `Set.difference` s'
  Abs.TDDataNV _ _ constructors -> Set.unions (fvOfConstructor s' <$> constructors) `Set.difference` s'
  Abs.TDDeclaration _ _ _ expr ->
    fvOfExpr s' expr `Set.difference` s'
  Abs.TDDeclarationNT _ _ expr ->
    fvOfExpr s' expr `Set.difference` s'
  where
    s' = s `Set.union` bvOfTopDef x

fvOfType :: (Show a) => FV (Abs.Type' a)
fvOfType s x = case x of
  Abs.TVar _ (Abs.LIdent v) -> Set.singleton v
  Abs.TApp _ _ types ->
    Set.unions (fvOfType s' <$> types) `Set.difference` s'
  Abs.TType {} -> Set.empty
  Abs.TBound _ _ type_ -> fvOfType s' type_ `Set.difference` s'
  where
    s' = s `Set.union` bvOfType x

fvOfConstructor :: (Show a) => FV (Abs.Constructor' a)
fvOfConstructor s x = case x of
  Abs.Constructor _ _ types -> Set.unions (fvOfType s <$> types) `Set.difference` s
  Abs.NullaryConstr {} -> Set.empty

fvOfArg :: (Show a) => FV (Abs.Arg' a)
fvOfArg s x = case x of
  Abs.Arg _ _ type_ -> fvOfType s type_ `Set.difference` s

fvOfExpr :: (Show a) => FV (Abs.Expr' a)
fvOfExpr s x = case x of
  Abs.ELet _ expr1 type_ expr2 expr3 ->
    Set.unions
      [ fvOfExpr s expr1,
        fvOfExpr s expr2,
        fvOfExpr s expr3,
        fvOfType s type_
      ]
      `Set.difference` s
  Abs.ELetNT _ expr1 expr2 expr3 ->
    Set.unions
      [ fvOfExpr s expr1,
        fvOfExpr s expr2,
        fvOfExpr s expr3
      ]
      `Set.difference` s
  Abs.EMatch _ expr matchbranchs ->
    Set.unions (fvOfExpr s expr : (fvOfMatchBranch s <$> matchbranchs)) `Set.difference` s
  Abs.EIf _ expr1 expr2 expr3 ->
    Set.unions
      [ fvOfExpr s expr1,
        fvOfExpr s expr2,
        fvOfExpr s expr3
      ]
      `Set.difference` s
  Abs.ELambda _ args expr ->
    Set.unions (fvOfExpr s expr : (fvOfArg s <$> args)) `Set.difference` s
  Abs.EList _ exprs -> Set.unions (fvOfExpr s <$> exprs) `Set.difference` s
  Abs.EId {} -> Set.empty
  Abs.EConstr {} -> Set.empty
  Abs.EIgnore {} -> Set.empty
  Abs.EApp _ expr exprs ->
    Set.unions (fvOfExpr s expr : (fvOfExpr s <$> exprs)) `Set.difference` s
  Abs.ELit {} -> Set.empty
  Abs.Neg _ expr -> fvOfExpr s expr `Set.difference` s
  Abs.Not _ expr -> fvOfExpr s expr `Set.difference` s
  Abs.EMul _ expr1 mulop expr2 -> Set.unions [fvOfExpr s expr1, fvOfExpr s expr2, fvOfMulOp s mulop] `Set.difference` s
  Abs.EAdd _ expr1 addop expr2 -> Set.unions [fvOfExpr s expr1, fvOfExpr s expr2, fvOfAddOp s addop] `Set.difference` s
  Abs.ERel _ expr1 relop expr2 -> Set.unions [fvOfExpr s expr1, fvOfExpr s expr2, fvOfRelOp s relop] `Set.difference` s
  Abs.EAnd _ expr1 expr2 -> Set.unions (fvOfExpr s <$> [expr1, expr2]) `Set.difference` s
  Abs.EOr _ expr1 expr2 -> Set.unions (fvOfExpr s <$> [expr1, expr2]) `Set.difference` s

fvOfMatchBranch :: (Show a) => FV (Abs.MatchBranch' a)
fvOfMatchBranch _ = const Set.empty

fvOfLiteral :: (Show a) => FV (Abs.Literal' a)
fvOfLiteral _ = const Set.empty

fvOfAddOp :: (Show a) => FV (Abs.AddOp' a)
fvOfAddOp _ = const Set.empty

fvOfMulOp :: (Show a) => FV (Abs.MulOp' a)
fvOfMulOp _ = const Set.empty

fvOfRelOp :: (Show a) => FV (Abs.RelOp' a)
fvOfRelOp _ = const Set.empty

tcFVOfType :: FV Type
tcFVOfType s x = case x of
  TCInt                      -> Set.empty
  TCVar v | v `Set.member` s -> Set.empty
  TCVar v                    -> Set.singleton v
  TCAny                      -> Set.empty
  TCApp _ ts                 -> Set.unions $ tcFVOfType s <$> ts
  TCBound vs t               -> tcFVOfType (s `Set.union` Set.fromList vs) t
  TCData {}                  -> Set.empty
