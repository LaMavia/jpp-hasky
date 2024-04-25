{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE QuasiQuotes     #-}

module Runtime.RTEval where

import           Control.Monad.Reader       (MonadReader, ReaderT (runReaderT),
                                             ask, asks, local)
import           Control.Monad.State        (StateT (runStateT), gets, modify)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Foldable              (foldlM)
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import           Data.String.Interpolate    (i)
import           Runtime.RTError            (RTError, rtThrow)


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
  | RTType !Type
  | -- | RTData TypeName [Arguments] { ConstrName: [ConstrArgs] }
    RTData !String ![String] !(Map.Map String DataConstr)

data Type
  = TInt
  | TVar !String
  | TIdent !String ![Type]
  | TUniv
  deriving (Eq)

newtype DataConstr = DConstr [Type] deriving (Eq)

instance Show RTVal where
  show (RTInt n) = [i|#{n} :: Int|]
  show (RTConstr t n args) = [i|#{t}.#{n}(#{argStrings})|]
    where argStrings = intercalate ", " $ show <$> args
  show (RTFunc _ args _) = [i|fun(#{argStrings})|]
      where argStrings = intercalate ", " $ show <$> args
  show (RTData t args _) = [i|data #{t} (#{argStrings})|]
    where argStrings = intercalate ", " $ show <$> args
  show (RTType t) = show t

instance Eq RTVal where
  RTInt a == RTInt b                     = a == b
  RTConstr ta ca aa == RTConstr tb cb ab = ta == tb && ca == cb && aa == ab
  RTType a == RTType b                   = a == b
  RTData ta aa ma == RTData tb ab mb     = ta == tb && aa == ab && ma == mb
  _ == _                                 = False

instance Show Type where
  show TInt = "Int"
  show (TVar name) = name
  show (TIdent t args) = [i|#{t}(#{argStrings})|]
      where argStrings = intercalate ", " $ show <$> args
  show TUniv = "$Type"

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

