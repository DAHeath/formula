{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
module Formula.Vocab where

import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Reader

import qualified Data.Map as M
import           Data.Map (Map)
import           Data.List.Split

class Monad m => MonadVocab m where
  fresh :: String -> m String
  fetch :: String -> m String

newtype VocabT m a = Vocab { getVocab :: StateT (Map String Int) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance MonadState s m => MonadState s (VocabT m) where
  state = lift . state

instance MonadReader s m => MonadReader s (VocabT m) where
  ask = lift ask
  local = mapVocabT . local
    where mapVocabT f = Vocab . mapStateT f . getVocab

instance Monad m => MonadVocab (VocabT m) where
  fresh n' = do
    let n = baseName n'
    Vocab (gets (M.lookup n) >>= \case
      Nothing -> modify (M.insert n 0)
      Just i  -> modify (M.insert n (i+1)))
    fetch n

  fetch n = Vocab (gets (M.lookup (baseName n)) >>= \case
    Nothing -> pure (baseName n)
    Just i -> pure (baseName n ++ "#" ++ show i))

instance MonadVocab m => MonadVocab (StateT s m) where
  fresh = lift . fresh
  fetch = lift . fetch

instance MonadVocab m => MonadVocab (ReaderT s m) where
  fresh = lift . fresh
  fetch = lift . fetch

type Vocab a = VocabT Identity a

runVocab :: Vocab a -> a
runVocab (Vocab a) = evalState a M.empty

runVocabT :: Monad m => VocabT m a -> m a
runVocabT (Vocab a) = evalStateT a M.empty

baseName :: String -> String
baseName = head . splitOn "#"
