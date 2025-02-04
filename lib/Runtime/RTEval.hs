{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE QuasiQuotes     #-}

module Runtime.RTEval where

import           Common
import           Control.Exception          (Exception, throw)
import           Control.Monad.Reader       (ReaderT (runReaderT), ask, asks,
                                             local)
import           Control.Monad.State        (StateT (runStateT), gets, modify)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import           Data.String.Interpolate    (i)


type RT = ReaderT RTEnv (StateT RTState (ExceptT UError IO))

runRT :: RT a -> IO (Either UError (a, RTState))
runRT m = runExceptT (runStateT (runReaderT m initialEnv) initialState)

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
  maybe (uThrow [i|getLoc: Key '#{key}' is not defined in the current env.|]) return l

getVar :: String -> RT RTVal
getVar key = do
  l <- getLoc key
  v <- gets (Map.lookup l . state)
  maybe (uThrow [i|getVar: Key '#{key}' (#{l})|]) return v

withFrame :: RT a -> RT a
withFrame m = do
  i0 <- gets loc
  res <- m
  modify (\s -> s {loc = i0})
  return res


type Location = Integer

type RTEnv = Map.Map String Location

type State = Map.Map Location RTVal

data RTState = RTState {state :: !State, loc :: !Location} deriving (Show)


initialEnv :: RTEnv
initialEnv = Map.empty

initialState :: RTState
initialState = RTState {state=Map.empty, loc=0}


data RTVal
  = RTInt !Integer
  | RTConstr !String !String ![RTVal]
  | RTFunc !RTEnv ![String] !(RT RTVal)

instance Show RTVal where
  show (RTInt n) = show n
  show (RTConstr t n args) = [i|#{t}.#{n}(#{argStrings})|]
    where argStrings = intercalate ", " $ show <$> args
  show (RTFunc _ args _) = [i|fun(#{argStrings})|]
      where argStrings = intercalate ", " $ show <$> args

instance Eq RTVal where
  RTInt a == RTInt b                     = a == b
  RTConstr ta ca aa == RTConstr tb cb ab = ta == tb && ca == cb && aa == ab
  _ == _                                 = False


newtype RTApplyException = RTApplyException { operator :: RTVal } deriving (Show)
instance Exception RTApplyException

rtApply :: RTVal -> [RTVal] -> RT RTVal
rtApply (RTFunc env argNames f) args = do
  let n = length argNames
  let m = length args
  env' <- local (const env) $ envSeq (zipWith alloc argNames args)
  if n == m then -- best case
    local (const env') f
  else if n > m then -- curry
    return $ RTFunc env' (drop m argNames) f
  else -- apply to the result
    do
      res <- local (const env') f
      rtApply res (drop n args)

rtApply (RTConstr t c []) args = return $ RTConstr t c args

rtApply x _ = throw $ RTApplyException {operator=x}

