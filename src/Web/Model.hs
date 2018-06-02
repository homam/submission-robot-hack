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
    , S.SubmissionErrorType, S.toSubmissionErrorType
    ) where

import           Control.Monad.IO.Class      (MonadIO (..), liftIO)
import           Control.Monad.Logger        (runNoLoggingT)
import           Control.Monad.Trans.Reader  (ReaderT (..))
import           Data.Text                   (Text, pack)
import qualified Data.Time                   as Time
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import           Control.Monad.Reader        (asks)
import           Control.Monad.Reader.Class  (MonadReader)
import           Control.Monad.Trans.Class   (MonadTrans, lift)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.Pool                   (Pool)
import           Web.AppState

import           Control.Arrow
import qualified Data.ByteString             as BS
import qualified Data.Text.Encoding          as E
import qualified Data.Time.Clock.POSIX       as POSIX
import qualified Database.Redis              as R
import qualified Network.URI                 as U
import qualified Sam.Robot                   as S


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
|]

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

runDb :: (MonadIO (t m), MonadReader AppState m, MonadTrans t) => ReaderT SqlBackend IO b -> t m b
runDb query = do
  run <- lift $ asks runSql
  liftIO (run query)

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

addMSISDNSubmission ::
     (MonadTrans t, MonadReader AppState m, MonadIO (t m))
  => Text -> Text -> Text -> Int -> Text -> Either (S.SubmissionError S.HttpException BS.ByteString) U.URI -> t m (Key MSISDNSubmission)
addMSISDNSubmission domain country handle offer msisdn res = do
  submissionId <- newSubmissionId
  let obj = addValidationRes res $ MSISDNSubmission (Just submissionId) country handle domain offer msisdn
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
  submissionErrorToText (S.APIError S.InvalidMSISDN     bs) = E.decodeUtf8 bs
  submissionErrorToText (S.APIError S.AlreadySubscribed bs) = E.decodeUtf8 bs
  submissionErrorToText (S.APIError S.ExceededMSISDNSubmissions bs) = E.decodeUtf8 bs
  submissionErrorToText (S.APIError S.InvalidPIN bs) = E.decodeUtf8 bs
  submissionErrorToText (S.APIError S.KeywordAndShortcodeNotFound bs) = E.decodeUtf8 bs
  submissionErrorToText (S.APIError S.UnknownError bs) = E.decodeUtf8 bs


