{-# LANGUAGE QuasiQuotes #-}

module Runtime.RTEval where

import           Control.Monad.Reader       (MonadReader, ReaderT (runReaderT),
                                             ask, asks, local)
import           Control.Monad.State        (StateT (runStateT), gets, modify)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Foldable              (foldlM)
import qualified Data.Map.Strict            as Map
import           Data.String.Interpolate    (i)
import           Runtime.RTError            (RTError, rtThrow)
import           Runtime.RTState            (Location, RTEnv,
                                             RTState (loc, state), initialEnv,
                                             initialState)
import           Runtime.RTVal              (RTVal)

type RT = ReaderT RTEnv (StateT RTState (ExceptT RTError IO))

runRT :: RT a -> IO (Either RTError (a, RTState))
runRT m = runExceptT (runStateT (runReaderT m initialEnv) initialState)

type RTEval a b = a -> RT b

allocEnv :: String -> RT RTEnv
allocEnv key = do
  l <- gets loc
  modify (\s -> s {loc = l + 1})
  asks $ Map.insert key l

allocState :: String -> RTVal -> RT ()
allocState key val = do
  l <- getLoc key
  modify (\s -> s {state = Map.insert l val (state s)})

alloc :: String -> RTVal -> RT RTEnv
alloc key val = envSeq [allocEnv key, allocState key val >> ask]

getLoc :: String -> RT Location
getLoc key = do
  l <- asks (Map.lookup key)
  maybe (rtThrow [i|getLoc: Key '#{key}' is not defined in the current env.|]) return l

getVar :: String -> RT RTVal
getVar key = do
  l <- getLoc key
  v <- gets (Map.lookup l . state)
  maybe (rtThrow [i|getVar: Key '#{key}' (#{l})|]) return v


envSeq :: (MonadReader r m) => [m r] -> m r
envSeq actions = do
  s0 <- ask
  foldlM (local . const) s0 actions
