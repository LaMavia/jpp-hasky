{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Runtime.RTError where

import Abs
  ( AddOp' (..),
    Arg' (..),
    BNFC'Position,
    Constructor' (..),
    Expr' (..),
    LIdent (..),
    Literal' (..),
    MatchBranch' (..),
    MulOp' (..),
    Program' (..),
    RelOp' (..),
    TopDef' (..),
    Type' (..),
    UIdent (..),
  )
import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Data.Bifunctor (bimap, first)
import Data.List (intercalate)
import Data.String.Interpolate (i)

type CallstackEntry = (BNFC'Position, String)

type RTResult = Either RTError

data RTError = RTError
  { callstack :: ![CallstackEntry],
    message :: !String
  }

instance Show RTError where
  show (RTError {callstack, message}) =
    unlines
      [ [i|Runtime error: '#{message}'.|],
        "Callstack:",
        unlines
          [ [i|#{r}:#{c} at #{place}|]
            | ((r, c), place) <- callstackWithPos
          ]
      ]
    where
      callstackWithPos = first (maybe ("?", "?") (bimap show show)) <$> callstack

rtThrow :: (MonadError RTError m) => String -> m a
rtThrow m = throwError $ RTError {callstack = [], message = m}

rtError :: CallstackEntry -> String -> RTError
rtError e m = RTError {callstack = [e], message = m}

rtCatch :: (MonadError RTError m) => CallstackEntry -> m a -> m a
rtCatch entry m = catchError m aux
  where
    aux e = throwError $ e {callstack = entry : callstack e}

placeOfLIdent :: Abs.LIdent -> String
placeOfLIdent x = case x of
  Abs.LIdent string -> string

placeOfUIdent :: Abs.UIdent -> String
placeOfUIdent x = case x of
  Abs.UIdent string -> string

placeOfProgram :: (Show a) => Abs.Program' a -> (a, String)
placeOfProgram x = case x of
  Abs.Program pos _ -> (pos, "<<program>>")

placeOfTopDef :: (Show a) => Abs.TopDef' a -> (a, String)
placeOfTopDef x = case x of
  Abs.TDDataV pos uident lidents _ ->
    let name = placeOfUIdent uident
        args = intercalate ", " $ placeOfLIdent <$> lidents
     in (pos, [i|type #{name} (#{args})|])
  Abs.TDDataNV pos uident _ ->
    let name = placeOfUIdent uident
     in (pos, [i|type #{name}|])
  Abs.TDDeclaration pos lident type_ _ ->
    let name = placeOfLIdent lident
        typeAdnot = placeOfType type_
     in (pos, [i|#{name} :: #{typeAdnot}|])
  Abs.TDDeclarationNT pos lident _ ->
    (pos, placeOfLIdent lident)

placeOfType :: (Show a) => Abs.Type' a -> (a, String)
placeOfType x = case x of
  Abs.TVar pos lident -> (pos, placeOfLIdent lident)
  Abs.TApp pos uident types ->
    let name = placeOfUIdent uident
        args = intercalate ", " $ snd . placeOfType <$> types
     in (pos, [i|#{name}(#{args})|])
  Abs.TType pos uident -> (pos, placeOfUIdent uident)
  Abs.TBound pos lidents type_ ->
    let vars = intercalate ", " $ placeOfLIdent <$> lidents
        typeExpr = placeOfType type_
     in (pos, [i|(#{vars}) => #{typeExpr}|])

placeOfConstructor :: (Show a) => Abs.Constructor' a -> (a, String)
placeOfConstructor x = case x of
  Abs.Constructor pos uident types ->
    let name = placeOfUIdent uident
        args = intercalate ", " $ snd . placeOfType <$> types
     in (pos, [i|#{name}(#{args})|])
  Abs.NullaryConstr pos uident ->
    (pos, placeOfUIdent uident)

placeOfArg :: (Show a) => Abs.Arg' a -> (a, String)
placeOfArg x = case x of
  Abs.Arg pos lident type_ ->
    let name = placeOfLIdent lident
        typeAdnot = snd $ placeOfType type_
     in (pos, [i|#{name} #{typeAdnot}|])

placeOfExpr :: (Show a) => Abs.Expr' a -> (a, String)
placeOfExpr x = case x of
  Abs.ELet pos expr1 type_ _ _ ->
    let expr = snd $ placeOfExpr expr1
        typeAdnot = snd $ placeOfType type_
     in (pos, [i|let #{expr} :: #{typeAdnot} |])
  Abs.ELetNT pos expr1 _ _ ->
    let expr = snd $ placeOfExpr expr1
     in (pos, [i|let #{expr}|])
  Abs.EMatch pos expr _ ->
    let val = snd $ placeOfExpr expr
     in (pos, [i|match #{val} with|])
  Abs.EIf pos expr1 _ _ ->
    let c = snd $ placeOfExpr expr1
     in (pos, [i|if #{c}|])
  Abs.ELambda pos args _ ->
    let a = intercalate ", " $ snd . placeOfArg <$> args
     in (pos, [i|fun (#{a})|])
  Abs.EList pos _ ->
    (pos, "<<list>>")
  Abs.EId pos lident ->
    (pos, placeOfLIdent lident)
  Abs.EConstr pos t c ->
    let tText = placeOfUIdent t
        cText = placeOfUIdent c
     in (pos, [i|#{tText}.#{cText}|])
  Abs.EIgnore pos ->
    (pos, "_")
  Abs.EApp pos expr exprs ->
    let f = snd $ placeOfExpr expr
        args = intercalate ", " $ snd . placeOfExpr <$> exprs
     in (pos, [i|#{f}(#{args})|])
  Abs.ELit pos literal ->
    (pos, snd $ placeOfLiteral literal)
  Abs.Neg pos _ ->
    (pos, "-")
  Abs.Not pos _ ->
    (pos, "!")
  Abs.EMul pos _ mulop _ ->
    (pos, snd $ placeOfMulOp mulop)
  Abs.EAdd pos _ addop _ ->
    (pos, snd $ placeOfAddOp addop)
  Abs.ERel pos _ relop _ ->
    (pos, snd $ placeOfRelOp relop)
  Abs.EAnd pos _ _ ->
    (pos, "&&")
  Abs.EOr pos _ _ ->
    (pos, "||")

placeOfMatchBranch :: (Show a) => Abs.MatchBranch' a -> (a, String)
placeOfMatchBranch x = case x of
  Abs.MBBranch pos expr1 _ -> (pos, [i|#{expr1} => ...|])

placeOfLiteral :: (Show a) => Abs.Literal' a -> (a, String)
placeOfLiteral x = case x of
  Abs.LInt pos integer -> (pos, show integer)

placeOfAddOp :: (Show a) => Abs.AddOp' a -> (a, String)
placeOfAddOp x = case x of
  Abs.Plus pos -> (pos, "+")
  Abs.Minus pos -> (pos, "-")

placeOfMulOp :: (Show a) => Abs.MulOp' a -> (a, String)
placeOfMulOp x = case x of
  Abs.Times pos -> (pos, "*")
  Abs.Div pos -> (pos, "/")
  Abs.Mod pos -> (pos, "%")

placeOfRelOp :: (Show a) => Abs.RelOp' a -> (a, String)
placeOfRelOp x = case x of
  Abs.LTH pos -> (pos, "<")
  Abs.LE pos -> (pos, "<=")
  Abs.GTH pos -> (pos, ">")
  Abs.GE pos -> (pos, ">=")
  Abs.EQU pos -> (pos, "==")
  Abs.NE pos -> (pos, "!=")
