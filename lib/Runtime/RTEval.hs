module Runtime.RTEval where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Runtime.RTError (RTError)
import Runtime.RTState (RTEnv, RTState)

type RTEval a b = a -> ReaderT RTEnv (StateT RTState (ExceptT RTError IO)) b
