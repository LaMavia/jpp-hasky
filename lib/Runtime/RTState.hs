module Runtime.RTState where

import           Abs
import qualified Data.Map.Strict as Map
import           Runtime.RTVal   (RTVal)

type Location = Int
type Env = Map.Map String Location
type State = Map.Map Location RTVal


data RTState
  = RTState
  { loc   :: Int
  , env   :: Env
  , state :: State
  }
