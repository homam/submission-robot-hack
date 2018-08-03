{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Web.Model
    (
      module Web.Model
    , Entity
    , S.SubmissionErrorType, S.toSubmissionErrorType
    ) where

import           Control.Monad.IO.Class           (MonadIO (..), liftIO)
import           Control.Monad.Logger             (runNoLoggingT)
import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Data.Aeson                       ((.=))
import qualified Data.Aeson                       as A
import           Data.Text                        (Text, pack)
import qualified Data.Time                        as Time
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Postgresql.Json
import           Database.Persist.TH


import           Control.Monad.Reader             (asks)
import           Control.Monad.Reader.Class       (MonadReader)
import           Control.Monad.Trans.Class        (MonadTrans, lift)
import           Control.Monad.Trans.Control      (MonadBaseControl)

import           Data.Pool                        (Pool)
import           Web.AppState

import           Control.Arrow
import qualified Data.ByteString                  as BS
import qualified Data.Text.Encoding               as E
import qualified Data.Time.Clock.POSIX            as POSIX
import qualified Database.Redis                   as R
import           GHC.Generics                     (Generic)
import qualified Network.URI                      as U
import qualified Robot.Sam                        as S


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
MSISDNSubmission sql=msisdn_submissions json
  Id
  submissionId Int Maybe
  creationTime Time.UTCTime default=now() MigrationOnly
  country Text
  handle Text
  domain Text
  offer Int
  msisdn Text
  queryString Json Maybe
  isValid Bool
  errorText Text Maybe
  finalUrl Text Maybe
  errorType S.SubmissionErrorType Maybe
  deriving Show

PINSubmission sql=pin_submissions json
  Id
  submissionId Int Maybe
  creationTime Time.UTCTime default=now() MigrationOnly
  msisdnSubmissionId MSISDNSubmissionId
  pin Text
  isValid Bool
  errorText Text Maybe
  finalUrl Text Maybe
  deriving Show

LastRSSales MigrationOnly sql=LastRSSales json
  msisdn Text
  country Text Maybe
  affiliateId Text Maybe
  creationDatetime Time.UTCTime Maybe
  Primary msisdn
  deriving Show
|]

-- share [mkPersist sqlSettings, mkSave "migrateNone"] [persistLowerCase|
-- LastRSSales json
--   msisdn Text Maybe
--   country Text Maybe
--   affiliateId Text Maybe
--   creationDatetime Time.UTCTime Maybe
--   deriving Show
-- |]

newtype AppStateM a = AppStateM {
    runAppStateM :: ReaderT AppState IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

newSubmissionId
  :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => t m Int
newSubmissionId =
  liftIO ((round :: Double -> Int) . (*1000000) . fromRational . toRational <$> POSIX.getPOSIXTime)

doMigrationsWithPool :: Pool SqlBackend -> IO ()
doMigrationsWithPool pool = flip runSqlPersistMPool pool $
    runMigration migrateAll

showMigrationsWithPool :: Pool SqlBackend -> IO [Text]
showMigrationsWithPool pool =
  flip runSqlPersistMPool pool $ showMigration migrateAll

runDb :: (MonadIO (t m), MonadReader AppState m, MonadTrans t) => ReaderT SqlBackend IO b -> t m b
runDb query = do
  run <- lift $ asks runSql
  liftIO (run query)

runDb'
  :: (MonadIO m, MonadReader AppState m)
  => ReaderT SqlBackend IO b
  -> m b
runDb' query = do
  run <- asks runSql
  liftIO $ run query


runRedisCommand :: (MonadIO (t m), MonadReader AppState m, MonadTrans t) => R.Redis b -> t m b
runRedisCommand command = do
  run <- lift $ asks runRedis
  liftIO (run command)

runApp ::
     (BaseBackend backend ~ SqlBackend, IsPersistBackend backend, MonadBaseControl IO m, MonadIO m)
  => ConnectionString -> (Pool backend -> IO a) -> m a
runApp connStr appf =
  runNoLoggingT $
    withPostgresqlPool connStr 10 $
    \pool -> liftIO $ appf pool

doMigrations :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => t m ()
doMigrations = runDb (runMigration migrateAll)

showMigrations :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => t m [Text]
showMigrations = runDb (showMigration migrateAll)

addMSISDNSubmission ::
     (MonadTrans t, MonadReader AppState m, MonadIO (t m))
  => Text -> Text -> Text -> Int -> Text -> [(Text, Text)] -> Either (S.SubmissionError S.HttpException BS.ByteString) U.URI -> t m (Key MSISDNSubmission)
addMSISDNSubmission domain country handle offer msisdn queryString res = do
  submissionId <- newSubmissionId
  let obj = addValidationRes res $ MSISDNSubmission (Just submissionId) country handle domain offer msisdn (Just $ Json $ A.object $ map (\(k, v) -> k .= v) queryString)
  runDb (insert obj)

addPINSubmission ::
     (MonadTrans t, MonadReader AppState m, MonadIO (t m))
  => Int -> Text -> Either (S.SubmissionError S.HttpException BS.ByteString) U.URI -> t m (Key PINSubmission)
addPINSubmission msisdnSubmissionKey pin res = do
  submissionId <- newSubmissionId
  runDb (insert $ addValidationRes res (\ i t f _ -> PINSubmission (Just submissionId) (toSqlKey $ fromIntegral msisdnSubmissionKey) pin i t f))

getMSISDNSubmission ::
    (MonadIO (t m), MonadReader AppState m, MonadTrans t, ToBackendKey SqlBackend MSISDNSubmission, Integral a)
  => a -> t m (Maybe MSISDNSubmission)
getMSISDNSubmission sid = runDb (get $ toSqlKey . fromIntegral  $ sid)

addValidationRes ::
      Either (S.SubmissionError S.HttpException BS.ByteString) U.URI
  -> (Bool -> Maybe Text -> Maybe Text -> Maybe S.SubmissionErrorType -> a)
  -> a
addValidationRes res f = f
  (const False ||| const True $ res)
  (Just . submissionErrorToText ||| const Nothing $ res)
  (const Nothing ||| Just . pack . ($ "") . U.uriToString id $ res)
  (Just . S.toSubmissionErrorType ||| const Nothing $ res)
 where
  submissionErrorToText (S.NetworkError e                 ) = pack $ show e
  submissionErrorToText (S.APIError _     bs)               = E.decodeUtf8 bs




-- getLatestRedshiftSales :: MonadIO m => ReaderT SqlBackend m [Entity LastRSSales]
getLatestRedshiftSales
  :: ( MonadIO m
     , MonadReader AppState m
     )
  => m [Entity LastRSSales]
getLatestRedshiftSales =
  -- runDb $ rawSql "SELECT ?? FROM public.latest_redshift_sales() as \"LastRSSales\" LIMIT ?" [toPersistValue (1000 :: Int)]

  runDb' $ rawSql "SELECT ?? \n\
    \ FROM dblink('redshift_server', $REDSHIFT$ \n\
    \ SELECT msisdn, country, affiliate_id, creation_datetime \n\
    \ from pacman.live p \n\
    \ where p.creation_datetime > dateadd(MINUTE, -10, SYSDATE) \n\
    \ and event_type = 'sale' \n\
    \ order by p.creation_datetime desc \n\
    \ limit 1000 \n\
    \ $REDSHIFT$) as \"LastRSSales\" (msisdn text, country text, affiliate_id text, creation_datetime timestamp) \n\
    \ "
    []
