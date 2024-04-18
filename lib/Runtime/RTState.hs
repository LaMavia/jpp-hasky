{-# LANGUAGE StrictData #-}

module Runtime.RTState where

import qualified Data.Map.Strict as Map
import Runtime.RTVal (RTVal)

type Location = Integer

type RTEnv = Map.Map String Location

type State = Map.Map Location RTVal

data RTState = RTState {state :: !State, loc :: !Location}
