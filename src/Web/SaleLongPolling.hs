{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.SaleLongPolling where

import           Control.Monad.Reader
import qualified Data.IORef           as IORef
import qualified Data.Set             as Set
import           Data.Text            (Text)
import           Web.AppState
import qualified Web.Model            as Model

newtype SaleLongPoollingT m a = SaleLongPoolling { unSaleLongPoolling :: ReaderT AppState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState, MonadTrans)

runSaleLongPoolling :: Monad m => AppState -> SaleLongPoollingT m a -> m a
runSaleLongPoolling s = (`runReaderT` s) . unSaleLongPoolling

loop :: SaleLongPoollingT IO [Model.Entity Model.LastRSSales]
loop = Model.getLatestRedshiftSales

loopExistingSales :: IORef.IORef (Set.Set (Text, Text)) -> SaleLongPoollingT IO Int
loopExistingSales ref = do
  sales <- Model.getExistingSales'
  let set = Set.fromList sales
  liftIO $ IORef.writeIORef ref set
  return (Set.size set)
