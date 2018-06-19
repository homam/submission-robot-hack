{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Visit(
    doMigrationsWeb
  , msisdnSubmissionWeb
  , msisdnSubmissionWebForMOFlow
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
import           Data.Bifunctor            (bimap)
import qualified Data.ByteString           as BS
import qualified Data.HashMap.Strict       as M
import           Data.Maybe                (fromMaybe, maybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack, toLower, unpack)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as E
import qualified Data.Text.Lazy            as TL
import           GHC.Generics
import           Network.HTTP.Types        (parseQueryText)
import           Network.HTTP.Types.Status (status500)
import qualified Network.URI               as U
import qualified Network.Wai               as W
import qualified Robot.Sam                 as S
import qualified Web.JewlModel             as JM
import           Web.Localization          (decrypt', encrypt', toLocalMSISDN)
import           Web.Model
import           Web.WebM

doMigrationsWeb :: WebMApp ()
doMigrationsWeb =
  getAndHead "/do_migrations" $
    TL.fromStrict . T.concat <$> showMigrations >>= text


toSubmissionResult :: Text -> Either (S.SubmissionError S.HttpException BS.ByteString) a -> SubmissionResult
toSubmissionResult submissionId' res = SubmissionResult {
    submissionId = submissionId'
  , isValid = const False ||| const True $ res
  , keyword =  Nothing
  , errorText = Just . submissionErrorToText . S.toSubmissionErrorType ||| const Nothing $ res
  , errorType = (Just . S.toSubmissionErrorType ||| const Nothing $ res)
  }
  where
    submissionErrorToText S.SENetworkError      = "Network Error"
    submissionErrorToText S.SEInvalidMSISDN   = "MSISDN Validation Failed"
    submissionErrorToText S.SEInvalidPIN = "PIN Validation Failed"
    submissionErrorToText S.SEAlreadySubscribed = "MSISDN is already subscribed"
    submissionErrorToText S.SEExceededMSISDNSubmissions = "Exceeded MSISDN Submissions"
    submissionErrorToText S.SEUnknownError = "Unknown Error"
    submissionErrorToText S.SEKeywordAndShortcodeNotFound = "Keyword and Shortcode not found"

toSubmissionResultForMOFlow :: Text -> Either (S.SubmissionError S.HttpException BS.ByteString) S.MOFlowSubmissionResult -> SubmissionResult
toSubmissionResultForMOFlow submissionId' res = SubmissionResult {
    submissionId = submissionId'
  , isValid = const False ||| const True $ res
  , keyword =  const Nothing ||| Just $ res
  , errorText = Just . submissionErrorToText . S.toSubmissionErrorType ||| const Nothing $ res
  , errorType = (Just . S.toSubmissionErrorType ||| const Nothing $ res)
  }
  where
    submissionErrorToText S.SENetworkError      = "Network Error"
    submissionErrorToText S.SEInvalidMSISDN   = "MSISDN Validation Failed"
    submissionErrorToText S.SEInvalidPIN = "PIN Validation Failed"
    submissionErrorToText S.SEAlreadySubscribed = "MSISDN is already subscribed"
    submissionErrorToText S.SEExceededMSISDNSubmissions = "Exceeded MSISDN Submissions"
    submissionErrorToText S.SEUnknownError = "Unknown Error"
    submissionErrorToText S.SEKeywordAndShortcodeNotFound = "Keyword and Shortcode not found"

data SubmissionResult = SubmissionResult {
      isValid      :: Bool
    , errorText    :: Maybe Text
    , keyword      :: Maybe S.MOFlowSubmissionResult
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

parseEncodedParams :: BS.ByteString -> [(Text, Text)]
parseEncodedParams bs =
  [ (k, fromMaybe "" v)
  | (k, v) <- parseQueryText bs
  ]

queryStringParams :: ActionT TL.Text WebM [(Text, Text)]
queryStringParams = parseEncodedParams . W.rawQueryString <$> request

msisdnSubmissionWeb :: WebMApp ()
msisdnSubmissionWeb =
  getAndHead "/submit_msisdn/:domain/:country/:handle/:offer" $
    join $ msisdnSubmissionAction False
                              <$> param "domain"
                              <*> param "country"
                              <*> param "handle"
                              <*> param "offer"
                              <*> (pack <$> (toLocalMSISDN <$> (unpack <$> param "country") <*> (unpack <$> param "msisdn")))
                              <*> (filter ((/= "msisdn") . fst) <$> queryStringParams)

msisdnSubmissionWebForMOFlow :: WebMApp ()
msisdnSubmissionWebForMOFlow =
  getAndHead "/submit_msisdn_mo/:domain/:country/:handle/:offer" $
    join $ msisdnSubmissionAction True
                              <$> param "domain"
                              <*> param "country"
                              <*> param "handle"
                              <*> param "offer"
                              <*> (pack <$> (toLocalMSISDN <$> (unpack <$> param "country") <*> (unpack <$> param "msisdn")))
                              <*> (filter ((/= "msisdn") . fst) <$> queryStringParams)

--TODO: break it down into two functions
msisdnSubmissionAction ::
     Bool
  -> Text
  -> Text
  -> Text
  -> Int
  -> Text
  -> [(Text, Text)]
  -> WebMAction ()
msisdnSubmissionAction isMOFlow domain country handle offer msisdn additionalParams =
  if not isMOFlow
    then do
      res <- liftIO $ S.runSubmission $ S.submitMSISDN (unpack domain) (unpack handle) (unpack country) offer (unpack msisdn) additionalParams'
      (sid :: Integer) <- fromIntegral . fromSqlKey <$> addMSISDNSubmission domain country handle offer msisdn additionalParams res
      let psid = pack . encrypt' . show $ sid
      addScotchHeader "SubmissionId" (TL.fromStrict psid)
      json $ toSubmissionResult psid res
    else do
      res <- liftIO $ S.runSubmission $ S.submitMSISDNForMOFlow (unpack domain) (unpack handle) (unpack country) offer (unpack msisdn) additionalParams'
      (sid :: Integer) <- fromIntegral . fromSqlKey <$> addMSISDNSubmission domain country handle offer msisdn additionalParams (castToUri <$> res)

      let psid = pack . encrypt' . show $ sid
      case res of
        Left (S.NetworkError _ ) ->
            json $ toSubmissionResultForMOFlow psid res
        Left (S.APIError _ _) ->
            json $ toSubmissionResultForMOFlow psid res
        Right _ -> do
          addScotchHeader "SubmissionId" (TL.fromStrict psid)
          json $ toSubmissionResultForMOFlow psid res

  where
    castToUri :: S.MOFlowSubmissionResult -> U.URI
    castToUri (S.MOFlowSubmissionResult keyword' shortcode) = fromMaybe U.nullURI $ U.parseURI $ "sms:" ++ (unpack keyword') ++ "&body=" ++ (unpack shortcode)

    additionalParams' = map (bimap unpack unpack) additionalParams

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
      (sid' :: Integer) <- fromIntegral . fromSqlKey <$> addPINSubmission sid pin res
      let epsid =  encrypt' . show $ sid'
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
