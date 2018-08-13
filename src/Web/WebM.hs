{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Web.WebM (
    module Web.WebM
  , DB.fromSqlKey
  , ActionT, ScottyT, ScottyError, param, text, json, params, get, post, options, redirect, request, status, header, headers, addHeader, addroute
) where

import           Control.Concurrent          (forkIO, killThread, threadDelay)
import           Control.Monad.Reader        (MonadIO, MonadReader)
import           Control.Monad.Trans.Reader  (ReaderT (..), runReaderT)
import           Data.Monoid                 ((<>))
import           Data.Pool                   ()

import qualified Data.Pool                   as P

import qualified Control.Concurrent.Async    as Async
import           Control.Exception           as Ex
import qualified Data.IORef                  as IORef
import qualified Data.Set                    as Set
import           Data.Text                   (Text, unpack)
import qualified Data.Text.Lazy              as TL
import qualified Database.Persist.Postgresql as DB
import qualified Database.PostgreSQL.Simple  as PS
import qualified Database.Redis              as R
import           Network.HTTP.Types          (StdMethod (..))
import qualified Network.Wai                 as W
import           Web.AppState
import           Web.Model
import qualified Web.SaleLongPolling         as SaleLongPolling
import           Web.Scotty                  (RoutePattern)
import           Web.Scotty.Trans            (ActionT, ScottyError, ScottyT,
                                              addHeader, addroute, get, header,
                                              headers, json, middleware,
                                              options, param, params, post,
                                              redirect, request, scottyT,
                                              status, text)
import           Web.Utils.LogMiddleware     (logAllMiddleware)


newtype WebM a = WebM { unWebM :: ReaderT AppState IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

type WebMAction a = ActionT TL.Text WebM a
type WebMApp a = ScottyT TL.Text WebM ()

shead :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
shead = addroute HEAD

getAndHead, postAndHead, getAndPostAndHead :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
getAndHead a b = get a b >> shead a b
postAndHead a b = post a b >> shead a b
getAndPostAndHead a b = get a b >> post a b >> shead a b

addScotchHeader :: Monad m => TL.Text -> TL.Text -> ActionT e m ()
addScotchHeader name = addHeader ("X-Scotch-" <> name)

runWeb :: AppState -> forall a. WebM a -> IO a
runWeb appState webm =
  runReaderT (unWebM webm) appState

runWebM :: R.ConnectInfo -> DB.ConnectionString -> DB.ConnectionString -> WebM b -> IO b
runWebM redisConnInfo jewlConnStr connStr a = do
  redisConn <- R.checkedConnect redisConnInfo
  jewlPool <- P.createPool (PS.connectPostgreSQL jewlConnStr) PS.close 1 10 10

  runApp connStr (\pool -> do
      (_, appState) <- mkAppState redisConn jewlPool pool
      runWeb appState a
    )

addServerHeader :: W.Middleware
addServerHeader =
  W.modifyResponse (W.mapResponseHeaders (("Server", "Scotch") :))

runWebServer :: Int -> R.ConnectInfo -> DB.ConnectionString -> DB.ConnectionString -> WebMApp b -> IO ()
runWebServer port redisConnInfo jewlConnStr connStr a = do
  redisConnE <- mapLeft "Cannot connect to Redis" <$> Ex.try (R.checkedConnect redisConnInfo)
  jewlPoolE <- mapLeft "Cannot connect to PostgreSQL jewl" <$> Ex.try (P.createPool (PS.connectPostgreSQL jewlConnStr) PS.close 1 10 10)

  case (,) <$> redisConnE <*> jewlPoolE of
    Left e -> putStrLn e
    Right (redisConn, jewlPool) ->
      runApp connStr (\pool -> do
        (loop, appState) <- mkAppState redisConn jewlPool pool
        _ <- Async.concurrently
          loop
          (scottyT port (runWeb appState) (middleware logAllMiddleware >> middleware addServerHeader >> a))
        return ()
      )

  where
    mapLeft :: String -> Either SomeException a -> Either String a
    mapLeft s (Left l)  = Left $ s ++ ":\n" ++ show l
    mapLeft _ (Right r) = Right r

mkAppState ::
     R.Connection
  -> P.Pool PS.Connection
  -> P.Pool DB.SqlBackend
  -> IO (IO (), AppState)
mkAppState redisConn jewlPool pool = do
        allSalesRef <- IORef.newIORef Set.empty
        let msisdnExists' country msisdn = Set.member (country, msisdn) <$> IORef.readIORef allSalesRef
            appState = AppState {
            echo = putStrLn . (unpack :: Text -> String)
          , runRedis = R.runRedis redisConn
          , runSql = (`DB.runSqlPool` pool)
          , runJewl = P.withResource jewlPool
          , msisdnExists = msisdnExists'
        }
        let  cacheExistingSales = SaleLongPolling.runSaleLongPoolling appState (SaleLongPolling.loopExistingSales allSalesRef)
        let  loop = cacheExistingSales >>= print >> threadDelay (120 * 1000 * 1000) >> loop
        let  handleAsyncError = either print (const $ putStrLn "Completed")
        return (handleAsyncError =<< Async.waitCatch =<< Async.async loop, appState)
