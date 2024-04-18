module Runtime.RTEval where

import Control.Monad.Reader (ask)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import qualified Data.Map.Strict as Map
import Runtime.RTError (RTError)
import Runtime.RTState (RTEnv, RTState (loc, state))
import Runtime.RTVal (RTVal)

type RT = ReaderT RTEnv (StateT RTState (ExceptT RTError IO))

type RTEval a b = a -> RT b

alloc :: String -> RTVal -> RT RTEnv
alloc key val = do
  l <- gets loc
  env <- ask
  modify (\s -> s {loc = l + 1, state = Map.insert l val (state s)})
  return $ Map.insert key l env
