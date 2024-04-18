{-# LANGUAGE QuasiQuotes #-}

module Runtime.RTEval where

import Control.Monad.Reader (MonadReader, ask, asks, local)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Runtime.RTError (RTError, rtThrow)
import Runtime.RTState (RTEnv, RTState (loc, state))
import Runtime.RTVal (RTVal)

type RT = ReaderT RTEnv (StateT RTState (ExceptT RTError IO))

type RTEval a b = a -> RT b

allocEnv :: String -> RT RTEnv
allocEnv key = do
  l <- gets loc
  modify (\s -> s {loc = l + 1})
  asks $ Map.insert key l

allocState :: String -> RTVal -> RT ()
allocState key val = do
  l <- asks (Map.lookup key)
  l' <- maybe (rtThrow [i|alloc: Key #{key} is not defined in the current env.|]) return l
  modify (\s -> s {state = Map.insert l' val (state s)})

alloc :: String -> RTVal -> RT RTEnv
alloc key val = envSeq [allocEnv key, allocState key val >> ask]

envSeq :: (MonadReader r m) => [m r] -> m r
envSeq actions = do
  s0 <- ask
  foldrM (\a s -> local (const s) a) s0 actions
