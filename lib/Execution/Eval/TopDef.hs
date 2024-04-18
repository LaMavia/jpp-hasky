module Execution.Eval.TopDef where

import Abs
import Control.Monad.Reader (MonadReader (reader))
import Runtime

evalTopDef :: RTEval TopDef ()
evalTopDef p@(TDDataV _ dataName args constrs) =
  rtCatch (placeOfTopDef p) $ do
    env <- reader
    return ()
