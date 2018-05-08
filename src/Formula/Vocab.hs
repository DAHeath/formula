{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Formula.Vocab where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable
import           Data.Data            (Data)
import           Data.List.Split
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Formula.Var

class Monad m =>
      MonadVocab m where
  fresh :: String -> m String
  fetch :: String -> m String
  table :: m (String -> String)

newtype VocabT m a = Vocab
  { getVocab :: StateT (Map String Int) m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance MonadState s m => MonadState s (VocabT m) where
  state = lift . state

instance MonadReader s m => MonadReader s (VocabT m) where
  ask = lift ask
  local = mapVocabT . local
    where
      mapVocabT f = Vocab . mapStateT f . getVocab

instance Monad m => MonadVocab (VocabT m) where
  fresh n' = do
    let n = baseName n'
    Vocab
      (gets (M.lookup n) >>= \case
         Nothing -> modify (M.insert n 0)
         Just i -> modify (M.insert n (i + 1)))
    fetch n
  fetch n =
    Vocab
      (gets (M.lookup (baseName n)) >>= \case
         Nothing -> pure (baseName n)
         Just i -> pure (baseName n ++ "#" ++ show i))
  table =
    Vocab $ do
      m <- get
      return
        (\n ->
           case M.lookup (unaliasName n) m of
             Nothing -> n
             Just i -> unaliasName n ++ "#" ++ show i)

instance MonadVocab m => MonadVocab (StateT s m) where
  fresh = lift . fresh
  fetch = lift . fetch
  table = lift table

instance MonadVocab m => MonadVocab (ReaderT s m) where
  fresh = lift . fresh
  fetch = lift . fetch
  table = lift table

type Vocab a = VocabT Identity a

runVocab :: Vocab a -> a
runVocab (Vocab a) = evalState a M.empty

runVocabT :: Monad m => VocabT m a -> m a
runVocabT (Vocab a) = evalStateT a M.empty

baseName :: String -> String
baseName = head . splitOn "#"

freshen :: (Data a, MonadVocab m) => Map Var Var -> a -> m a
freshen tab x = subst <$> aliasMap <*> pure x
  where
    aliasMap = foldrM addAlias tab (varSet x)
    addAlias v m =
      if v `elem` M.keys tab
        then pure m
        else do
          v' <- fresh' v
          pure $ M.insert v v' m

fresh' :: MonadVocab m => Var -> m Var
fresh' v = v & varName %%~ fresh

fetch' :: MonadVocab m => Var -> m Var
fetch' v = v & varName %%~ fetch

table' :: MonadVocab m => m (Var -> Var)
table' = do
  t <- table
  pure (\v -> v & varName %~ t)

unaliasedVarName :: Var -> String
unaliasedVarName = unaliasName . view varName

unaliasName :: String -> String
unaliasName n =
  case splitOn "#" n of
    [x, _] -> x
    [x] -> x
    _ -> error "invalid variable name has more than 1 instance of '#'"

unaliasedVar :: Var -> Var
unaliasedVar v = v & varName %~ unaliasName
