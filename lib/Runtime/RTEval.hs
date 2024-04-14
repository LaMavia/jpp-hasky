module Runtime.RTEval where

import Runtime.RTState
import Runtime.RTError
import Runtime.RTVal

type RTEval a b = a -> RTState -> RTResult b

