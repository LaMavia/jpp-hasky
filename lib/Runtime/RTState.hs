{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}


module Runtime.RTState where

import qualified Data.Map.Strict as Map
import           Runtime.RTVal   (RTVal)


type Location = Integer

type RTEnv = Map.Map String Location

type State = Map.Map Location RTVal

data RTState = RTState {state :: !State, loc :: !Location} deriving (Show)


initialEnv :: RTEnv
initialEnv = Map.empty

initialState :: RTState
initialState = RTState {state=Map.empty, loc=0}
