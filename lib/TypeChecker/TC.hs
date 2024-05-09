{-# LANGUAGE QuasiQuotes #-}

module TypeChecker.TC where

import qualified Abs
import           Common                  (UError, envSeq, showSepList, uThrow)
import           Control.Monad.Except    (ExceptT, runExceptT)
import           Control.Monad.Identity  (Identity (runIdentity))
import           Control.Monad.Reader    (MonadReader (ask, local),
                                          ReaderT (runReaderT), asks)
import           Control.Monad.State     (StateT (runStateT), gets, modify)
import           Data.Foldable           (foldlM)
import           Data.List               (intercalate, intersperse)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (isJust)
import           Data.String.Interpolate (i)


type Location = Integer

type TCEnv = Map.Map String Location

data TCState = TCState { state :: !(Map.Map Location Type), loc :: !Location }

data Type
  = TCInt
  | TCVar !String
  | TCApp !String ![Type]
  | TCAny
  | TCBound ![String] !Type
  | TCData !String ![String] !(Map.Map String [Type]) !([Type] -> Bool)

instance Eq Type where
  TCAny == _                               = True
  _ == TCAny                               = True
  TCVar x == TCVar y                       = x == y
  TCApp tx tsx == TCApp ty tsy             = tx == ty && tsx == tsy
  TCData tx asx mx _ == TCData ty asy my _ = tx == ty && asx == asy && mx == my
  _ == _                                   = False

instance Show Type where
  show TCInt = "Int"
  show (TCVar x) = x
  show (TCBound args t) =
    let argsString = showSepList ", " args
    in [i|(#{argsString}) => #{t}|]
  show TCAny = "@Any"
  show (TCApp t ts) =
    let tsString = showSepList ", " ts in [i|#{t}(#{tsString})|]
  show (TCData t args _ _) =
    let argsString = intercalate ", " args
    in [i|#{t}(#{argsString})|]


type TC = ReaderT TCEnv (StateT TCState (ExceptT UError Identity))

type TCChecker a b = a -> TC (b, a)

runTC :: TC a -> Either UError (a, TCState)
runTC m = runIdentity $ runExceptT (runStateT (runReaderT m initialEnv) initialState)

allocEnv :: String -> TC TCEnv
allocEnv key = do
  l <- gets loc
  modify (\s -> s {loc = l + 1})
  asks $ Map.insert key l

allocState :: String -> Type -> TC ()
allocState key val = do
  l <- getLoc key
  modify (\s -> s {state = Map.insert l val (state s)})

alloc :: String -> Type -> TC TCEnv
alloc key val = envSeq [allocEnv key, allocState key val >> ask]

getLoc :: String -> TC Location
getLoc key = do
  l <- asks (Map.lookup key)
  maybe (uThrow [i|getLoc: Key '#{key}' is not defined in the current env.|]) return l

getVar :: String -> TC Type
getVar key = do
  l <- getLoc key
  v <- gets (Map.lookup l . state)
  maybe (uThrow [i|getVar: Key '#{key}' (#{l})|]) return v

isDefined :: String -> TC Bool
isDefined key = do
  l <- asks (Map.lookup key)
  return $ isJust l

withFrame :: TC a -> TC a
withFrame m = do
  i0 <- gets loc
  res <- m
  modify (\s -> s {loc = i0})
  return res


initialEnv :: TCEnv
initialEnv = Map.empty

initialState :: TCState
initialState = TCState {state=Map.empty, loc=0}

mapTCEnv :: TCChecker a TCEnv -> TCChecker [a] TCEnv
mapTCEnv c ins = do
  env <- ask
  (env', outs) <- foldlM aux (env, []) (c <$> ins)
  return (env', reverse outs)
  where
    aux :: (TCEnv, [a]) -> TC (TCEnv, a) -> TC (TCEnv, [a])
    aux (env, us) m = do
      (env', u) <- local (const env) m
      return (env', u:us)

astOfType :: Type -> Abs.BNFC'Position -> TC Abs.Type
astOfType TCInt pos = return $ Abs.TType pos (Abs.UIdent "Int")
astOfType (TCVar x) pos = return $ Abs.TVar pos (Abs.LIdent x)
astOfType (TCApp t ts) pos = do
  tsAsts <- mapM (`astOfType` Nothing) ts
  return $ Abs.TApp pos (Abs.UIdent t) tsAsts
astOfType t _ = uThrow [i|Type «#{t}» doesn't translate to AST.|]
