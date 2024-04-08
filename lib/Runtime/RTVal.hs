module Runtime.RTVal where

import Runtime.RTError (RTError)

type RTResult = Either RTError RTVal

data RTVal 
  = RTInt Int
  | RTConstr String [RTVal]
  | RTFunc ([RTVal] -> RTResult)
