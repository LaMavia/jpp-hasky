{-# LANGUAGE StrictData #-}

module Runtime.RTState where

import qualified Data.Map.Strict as Map
import Runtime.RTVal (RTVal)

type Location = Integer

type State = Map.Map Location RTVal

type RTEnv = Map.Map String Location

data RTState = RTState
  { loc :: !Integer,
    state :: !State
  }
