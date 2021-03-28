module App (App(..), AppState(..), runApp) where

import           Control.Monad.Reader
import qualified Data.HashTable.IO    as H
import           Data.Hashable        (hash)


newtype AppState = AppState { unAppState :: H.BasicHashTable String Int }

newtype AppM a = AppM { runApp :: ReaderT AppState IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppState)

class Monad m => App m where
  lookupEntry :: String -> m (Maybe Int)
  mutateEntry :: String -> (Maybe Int -> Maybe Int) -> m ()
  insertEntry :: String -> String -> m ()
  logLine     :: String -> m ()

instance App AppM where
  lookupEntry str = do
    h <- unAppState <$> ask
    liftIO $ H.lookup h str

  mutateEntry str f = do
    h <- unAppState <$> ask
    liftIO $ H.mutate h str (\m -> (f m, ()))

  insertEntry str1 str2 = do
    h <- unAppState <$> ask
    liftIO $ H.insert h str1 (hash str2)

  logLine = liftIO . putStrLn

instance (Monad (t m), MonadTrans t, App m) => App (t m) where
  lookupEntry     = lift . lookupEntry
  mutateEntry s   = lift . mutateEntry s
  insertEntry str = lift . insertEntry str
  logLine         = lift . logLine
