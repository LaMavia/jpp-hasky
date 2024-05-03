module Common.Utils where
import           Control.Monad.Reader (MonadReader (ask, local))
import           Data.Foldable        (foldlM)

envSeq :: (MonadReader r m) => [m r] -> m r
envSeq actions = do
  s0 <- ask
  foldlM (local . const) s0 actions
