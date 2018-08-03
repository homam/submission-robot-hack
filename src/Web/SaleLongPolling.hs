{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.SaleLongPolling where

import           Control.Concurrent       (forkIO, threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad            (forever)
import           Control.Monad.Reader
import           Web.AppState
import qualified Web.Model                as Model

-- threadId <- forkIO $ forever $ do
--   threadDelay (60 * 1000 * 1000) -- one minute in microseconds, not milliseconds like in Javascript!
--   doWhateverYouLikeHere

newtype SaleLongPoollingT m a = SaleLongPoolling { unSaleLongPoolling :: ReaderT AppState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState, MonadTrans)

runSaleLongPoolling :: Monad m => AppState -> SaleLongPoollingT m a -> m a
runSaleLongPoolling s = (`runReaderT` s) . unSaleLongPoolling

loop :: SaleLongPoollingT IO [Model.Entity Model.LastRSSales]
loop = do
  sales <- Model.getLatestRedshiftSales
  -- let s' = map _ sales
  return sales
