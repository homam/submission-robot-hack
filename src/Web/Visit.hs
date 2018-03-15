{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Visit(
    doMigrationsWeb
  , msisdnSubmissionWeb
  , pinSubmissionWeb
  , msisdnExistsWeb
  , SubmissionResult (..)
)
where

import           Control.Arrow
import           Control.Monad             (join)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Zip         (mzip)
import qualified Data.Aeson                as A
import qualified Data.Aeson.Types          as AT
import qualified Data.ByteString           as BS
import qualified Data.HashMap.Strict       as M
import           Data.Maybe                (maybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack, toLower, unpack)
import qualified Data.Text.Encoding        as E
import qualified Data.Text.Lazy            as TL
import           GHC.Generics
import           Network.HTTP.Types.Status (status500)
import qualified Network.URI               as U
import qualified Sam.Robot                 as S
import qualified Web.JewlModel             as JM
import           Web.Localization          (decrypt', encrypt', toLocalMSISDN)
import           Web.Model
import           Web.WebM

doMigrationsWeb :: WebMApp ()
doMigrationsWeb =
  getAndHead "/do_migrations" $
    doMigrations >> text "done!"


toSubmissionResult :: Text -> Either (S.SubmissionError S.HttpException BS.ByteString) U.URI -> SubmissionResult
toSubmissionResult submissionId res = SubmissionResult {
    submissionId = submissionId
  , isValid = const False ||| const True $ res
  , errorText = Just . submissionErrorToText ||| const Nothing $ res
  , errorType = (Just . S.toSubmissionErrorType ||| const Nothing $ res)
  } where
    submissionErrorToText (S.NetworkError e)      = pack $ show e
    submissionErrorToText (S.ValidationError _)   = "Validation Failed"
    submissionErrorToText (S.AlreadySubscribed _) = "MSISDN is already subscribed"
    submissionErrorToText (S.ExceededMSISDNSubmissions _) = "Exceeded MSISDN Submissions"

data SubmissionResult = SubmissionResult {
      isValid      :: Bool
    , errorText    :: Maybe Text
    , submissionId :: Text
    , errorType    :: Maybe S.SubmissionErrorType
  } deriving (Eq, Ord, Show, Read, Generic)

instance A.ToJSON SubmissionResult
instance A.FromJSON SubmissionResult

data FinalResult = FinalResult { finalUrl :: Text, finalSubmissionResult :: SubmissionResult }
instance A.ToJSON FinalResult where
  toJSON (FinalResult u s) = if isValid s then AT.Object (M.insert "finalUrl" v o) else A.toJSON s where
    AT.Object o = A.toJSON s
    v           = A.toJSON u

msisdnSubmissionWeb :: WebMApp ()
msisdnSubmissionWeb =
  getAndHead "/submit_msisdn/:domain/:country/:handle/:offer" $
    join $ msisdnSubmissionAction <$> param "domain" <*> param "country" <*> param "handle" <*> param "offer"
    <*> (pack <$> (toLocalMSISDN <$> (unpack <$> param "country") <*> (unpack <$> param "msisdn")))

msisdnSubmissionAction :: Text -> Text -> Text -> Int -> Text -> WebMAction ()
msisdnSubmissionAction domain country handle offer msisdn = do
  res <- liftIO $ S.runSubmission $ S.submitMSISDN (unpack domain) (unpack handle) (unpack country) offer (unpack msisdn)
  sid <- fromIntegral . fromSqlKey <$> addMSISDNSubmission domain country handle offer msisdn res
  let psid = pack . encrypt' . show $ sid
  addScotchHeader "SubmissionId" (TL.fromStrict psid)
  json $ toSubmissionResult psid res

pinSubmissionWeb :: WebMApp ()
pinSubmissionWeb =
  getAndHead "/submit_pin/" $ do
    msid <- decrypt' <$> param "sid"
    case msid of
      Left e    -> text (TL.pack $ show e)
      Right sid -> join $ pinSubmissionAction <$> (return $ read sid) <*> param "pin"

pinSubmissionAction :: Int -> Text -> WebMAction ()
pinSubmissionAction sid pin = do
  submission' <- getMSISDNSubmission sid
  let url' = (U.parseURI . unpack) =<< mSISDNSubmissionFinalUrl =<< submission'
  case mzip url' submission' of
    Just (url, submission) -> do
      let country = mSISDNSubmissionCountry submission
      res <- liftIO $ S.runSubmission $ S.submitPIN (E.encodeUtf8 pin) url
      sid <- fromIntegral . fromSqlKey <$> addPINSubmission sid pin res
      let epsid =  encrypt' . show $ sid
      addScotchHeader "SubmissionId" (TL.pack epsid)
      json FinalResult { finalUrl = finalUrl' country , finalSubmissionResult = toSubmissionResult (pack epsid) res }
    Nothing ->
      let err = TL.pack $ maybe "MSISDN Submission" (const "Final URL") submission' in
      status status500 >> text ("No " <> err <> " was Found for the Given sid: " <> TL.pack (show sid))
  where
    finalUrl' = finalUrl'' . toLower where
      finalUrl'' "gr" = "http://gr.mobiworldbiz.com/?uid=fdf098fcc6&uip=2.84.0.0"
      finalUrl'' "iq" = "http://iq.mobbfun.com/?uid=fdf098fcc6"
      finalUrl''  _   = "http://gr.mobiworldbiz.com/?uid=fdf098fcc6&uip=2.84.0.0"



msisdnExistsWeb :: WebMApp ()
msisdnExistsWeb =
  getAndHead "/check_msisdn_active_subscription/:country/" $ do
    msisdn' <- param "msisdn"
    country' <- param "country"
    res <- JM.runJewlDb $ JM.msisdnStatus country' msisdn'
    json res
