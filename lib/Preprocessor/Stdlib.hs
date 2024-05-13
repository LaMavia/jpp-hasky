{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}

module Preprocessor.Stdlib where

import           Abs                     (Program, Program' (Program), TopDef)
import           Common                  (envSeq)
import           Control.Exception       (Exception, throw)
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Control.Monad.Reader    (MonadReader (ask))
import           Data.Bifunctor          (Bifunctor (second))
import           Data.Data               (Typeable)
import qualified Data.Map.Strict         as Map
import           Data.String.Interpolate (i)
import           Par                     (myLexer, pProgram)
import           Preprocessor.Desugar    (desugarProgram)
import           Runtime                 (RT, RTEnv, RTVal (RTFunc), alloc,
                                          getVar, rtcVoid)
import qualified TypeChecker.TC          as TC
import           TypeChecker.TC          (TC, TCEnv,
                                          Type (TCApp, TCBound, TCData, TCVar))

newtype StdlibCompilationError = StdlibCompilationError String deriving (Show, Typeable)

instance Exception StdlibCompilationError

definableStdlib :: [TopDef]
definableStdlib =
  case stdlib of
    Left e                  -> throw $ StdlibCompilationError e
    Right (Program _ stmts) -> stmts
  where
    stdlib = second desugarProgram $ pProgram $ myLexer [i|
      type Bool = True | False ;;

      type Void = V ;;

      type List(a)
        = Nil
        | Cons(a, List(a))
      ;;

      type Maybe(a)
        = Nothing
        | Just(a)
      ;;

      head :: (a) => Fun(List(a), Maybe(a))
        = fun (xs List(a)) ->
          match xs with (
            | List.Cons(h, _) -> Maybe.Just(h)
            | _               -> Maybe.Nothing()
          )
        ;;

      tail :: (a) => Fun(List(a), Maybe(List(a)))
        = fun (xs List(a)) ->
          match xs with (
            | List.Cons(_, t) -> Maybe.Just(t)
            | _               -> Maybe.Nothing()
          )
        ;;

      empty :: (a) => Fun(List(a), Bool)
        = fun (xs List(a)) ->
          match xs with (
            | List.Nil() -> Bool.True
            | _          -> Bool.False
          )
        ;;

      map :: (a, b) => Fun(Fun(a, b), List(a), List(b))
        = fun (f Fun(a, b), xs List(a)) ->
          match xs with (
            | List.Nil()       -> []
            | List.Cons(h, t)  -> List.Cons(f(h), map(f, t))
          )
        ;;

      foldr :: (a, b) => Fun(Fun(a, b, b), b, List(a), b)
        = fun (f Fun(a, b, b), u0 b, xs List(a)) ->
          match xs with (
            | List.Nil() -> u0
            | List.Cons(h, tl) ->
              let u :: b = foldr(f, u0, tl)
              in f(h, u)
          )
        ;;
    |]

prependStdlib :: Program -> Program
prependStdlib (Program pos stmts) = Program pos (definableStdlib <> stmts)

runTypePrelude :: TC TCEnv
runTypePrelude = do
  envSeq [decInt, decFun, defPrint]
  where
    defPrint =
      -- print :: (a) => Fn(a, Void())
      let f = TCBound ["a"] (TCApp "Fn" [TCVar "a", TCApp "Void" []])
      in TC.alloc "print" f
    decInt =
      let int = TCData "Int" [] Map.empty ([] ==)
      in TC.alloc "Int" int
    decFun =
      -- Don't allow for `Fun()` or `Fun(a)`.
      -- The latter should have been replaced with `a` during typechecking.
      let fun = TCData "Fn" ["a", "b"] Map.empty (\case [_,_] -> True; _ -> False)
      in TC.alloc "Fn" fun

runPrelude :: RT RTEnv
runPrelude = do
  envSeq [defPrint]
  where
    defPrint = do
      env <- ask
      let f = RTFunc env ["x"] (do x <- getVar "x"; (liftIO . print) x >> return rtcVoid)
      alloc "print" f

