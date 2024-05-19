module Common.Utils where
import           Control.Monad.Reader (MonadReader (ask, local))
import           Data.Foldable        (foldlM)
import           Data.List            (intercalate, unfoldr, union)
import           Debug.Trace

envSeq :: (MonadReader r m) => [m r] -> m r
envSeq actions = do
  s0 <- ask
  foldlM (local . const) s0 actions

withEnv :: (MonadReader r m) => r -> m a -> m a
withEnv = local . const

-- |
-- >>> findDuplicates [1,2,3,4,1,2,6,7,4]
-- [1,2,4]
-- >>> findDuplicates []
-- []
findDuplicates :: Eq a => [a] -> [a]
findDuplicates = unfoldr firstDuplicates
  where
    firstDuplicates [] = Nothing
    firstDuplicates [_] = Nothing
    firstDuplicates (y:ys)
      | y `elem` ys = Just (y, filter (/= y) ys)
      | otherwise = firstDuplicates ys

showSepList :: Show a => String -> [a] -> String
showSepList sep xs = intercalate sep $ show <$> xs

unions :: Eq a => [[a]] -> [a]
unions = foldr union []

sniff :: String -> a -> a
sniff _ = id
