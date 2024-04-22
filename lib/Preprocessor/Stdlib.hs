{-# LANGUAGE QuasiQuotes #-}

module Preprocessor.Stdlib where

import           Abs                     (Program, Program' (Program), TopDef)
import           Control.Exception       (Exception, throw)
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Data.Bifunctor          (Bifunctor (second))
import           Data.Data               (Typeable)
import           Data.String.Interpolate (i)
import           Par                     (myLexer, pProgram)
import           Preprocessor.Desugar    (desugarProgram)
import           Runtime                 (RT, RTEnv, RTVal (RTFunc), alloc,
                                          envSeq, rtcVoid)

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

runPrelude :: RT RTEnv
runPrelude = do
  envSeq [defPrint]
  where
    defPrint = alloc "print" $ RTFunc (\vs -> do mapM_ (liftIO . print) vs >> return rtcVoid)

