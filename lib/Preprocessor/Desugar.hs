{-# LANGUAGE LambdaCase #-}

module Preprocessor.Desugar where

import           Abs

positionOfGenericExpr :: Abs.Expr' a -> a
positionOfGenericExpr = \case
  ELet p _ _ _ _ -> p
  ELetNT p _ _ _ -> p
  EMatch p _ _ -> p
  EIf p _ _ _ -> p
  ELambda p _ _ -> p
  EList p _ -> p
  EId p _ -> p
  EConstr p _ _ -> p
  EIgnore p -> p
  EApp p _ _ -> p
  ELit p _ -> p
  Neg p _ -> p
  Not p _ -> p
  EMul p _ _ _ -> p
  EAdd p _ _ _ -> p
  ERel p _ _ _ -> p
  EAnd p _ _ -> p
  EOr p _ _ -> p

desugarLIdent :: Abs.LIdent -> Abs.LIdent
desugarLIdent x = x

desugarUIdent :: Abs.UIdent -> Abs.UIdent
desugarUIdent x = x

desugarProgram :: (Show a) => Abs.Program' a -> Abs.Program' a
desugarProgram x = case x of
  Abs.Program pos topdefs -> Abs.Program pos (desugarTopDef <$> topdefs)

desugarTopDef :: (Show a) => Abs.TopDef' a -> Abs.TopDef' a
desugarTopDef x = case x of
  Abs.TDDataV pos uident lidents constructors ->
    Abs.TDDataV pos uident lidents (desugarConstructor <$> constructors)
  Abs.TDDataNV pos uident constructors ->
    desugarTopDef (Abs.TDDataV pos uident [] constructors)
  Abs.TDDeclaration pos lident type_ expr ->
    Abs.TDDeclaration pos lident (desugarType type_) (desugarExpr expr)
  Abs.TDDeclarationNT pos lident expr ->
    Abs.TDDeclarationNT pos lident (desugarExpr expr)

desugarType :: (Show a) => Abs.Type' a -> Abs.Type' a
desugarType x = case x of
  Abs.TApp pos (Abs.UIdent "Fun") types ->
    let types' = desugarType <$> types
    in foldr1 (\t u -> Abs.TApp pos (Abs.UIdent "Fn") [t, u]) types'
  Abs.TApp pos uident types ->
    Abs.TApp pos uident (desugarType <$> types)
  Abs.TBound pos lidents type_ ->
    Abs.TBound pos lidents (desugarType type_)
  Abs.TType pos t ->
    Abs.TApp pos t []
  y -> y

desugarConstructor :: (Show a) => Abs.Constructor' a -> Abs.Constructor' a
desugarConstructor x = case x of
  Abs.Constructor pos uident types ->
    Abs.Constructor pos uident (desugarType <$> types)
  y -> y

desugarArg :: (Show a) => Abs.Arg' a -> Abs.Arg' a
desugarArg x = case x of
  Abs.Arg pos lident type_ ->
    Abs.Arg pos lident (desugarType type_)

desugarExpr :: (Show a) => Abs.Expr' a -> Abs.Expr' a
desugarExpr x = case x of
  Abs.ELet pos expr1 type_ expr2 expr3 ->
    Abs.ELet pos (desugarExpr expr1) (desugarType type_) (desugarExpr expr2) (desugarExpr expr3)
  Abs.ELetNT pos expr1 expr2 expr3 ->
    Abs.ELetNT pos (desugarExpr expr1) (desugarExpr expr2) (desugarExpr expr3)
  Abs.EMatch pos expr matchbranchs ->
    Abs.EMatch pos (desugarExpr expr) (desugarMatchBranch <$> matchbranchs)
  Abs.EIf pos expr1 expr2 expr3 ->
    Abs.EIf pos (desugarExpr expr1) (desugarExpr expr2) (desugarExpr expr3)
  Abs.ELambda pos args expr ->
    Abs.ELambda pos (desugarArg <$> args) (desugarExpr expr)
  Abs.EList pos exprs ->
    let aux e u =
          let ePos = positionOfGenericExpr e
           in Abs.EApp ePos (Abs.EConstr ePos (Abs.UIdent "List") (Abs.UIdent "Cons")) [e, u]
     in foldr (aux . desugarExpr) (Abs.EApp pos (Abs.EConstr pos (Abs.UIdent "List") (Abs.UIdent "Nil")) []) exprs
  Abs.EId pos lident -> Abs.EId pos lident
  Abs.EConstr pos t c -> Abs.EConstr pos t c
  Abs.EIgnore pos -> Abs.EIgnore pos
  Abs.EApp pos expr exprs ->
    Abs.EApp pos (desugarExpr expr) (desugarExpr <$> exprs)
  Abs.ELit pos literal -> Abs.ELit pos (desugarLiteral literal)
  Abs.Neg pos expr -> Abs.Neg pos (desugarExpr expr)
  Abs.Not pos expr -> Abs.Not pos (desugarExpr expr)
  Abs.EMul pos expr1 mulop expr2 -> Abs.EMul pos (desugarExpr expr1) mulop (desugarExpr expr2)
  Abs.EAdd pos expr1 addop expr2 -> Abs.EAdd pos (desugarExpr expr1) addop (desugarExpr expr2)
  Abs.ERel pos expr1 relop expr2 -> Abs.ERel pos (desugarExpr expr1) relop (desugarExpr expr2)
  Abs.EAnd pos expr1 expr2 -> Abs.EAnd pos (desugarExpr expr1) (desugarExpr expr2)
  Abs.EOr pos expr1 expr2 -> Abs.EOr pos (desugarExpr expr1) (desugarExpr expr2)

desugarMatchBranch :: (Show a) => Abs.MatchBranch' a -> Abs.MatchBranch' a
desugarMatchBranch x = case x of
  Abs.MBBranch pos expr1 expr2 -> Abs.MBBranch pos (desugarExpr expr1) (desugarExpr expr2)

desugarLiteral :: (Show a) => Abs.Literal' a -> Abs.Literal' a
desugarLiteral x = x

desugarAddOp :: (Show a) => Abs.AddOp' a -> Abs.AddOp' a
desugarAddOp x = x

desugarMulOp :: (Show a) => Abs.MulOp' a -> Abs.MulOp' a
desugarMulOp x = x

desugarRelOp :: (Show a) => Abs.RelOp' a -> Abs.RelOp' a
desugarRelOp x = x
