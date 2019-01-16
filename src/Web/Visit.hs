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
  , latestSalesWeb
  , SubmissionResult (..)
)
where

import           Control.Arrow
import           Control.Monad             (join)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader
import           Control.Monad.Zip         (mzip)
import qualified Data.Aeson                as A
import qualified Data.Aeson.Types          as AT
import           Data.Bifunctor            (bimap)
import qualified Data.ByteString           as BS
import qualified Data.HashMap.Strict       as M
import           Data.Maybe                (fromMaybe, maybe)
import           Data.Monoid               ((<>))
import           Data.String               (IsString)
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
import qualified Robot.SamMOXHR as MO
import           Web.AppState
import qualified Web.JewlModel             as JM
import           Web.Localization          (decrypt', encrypt', toLocalMSISDN)
import           Web.Model
import           Web.WebM

doMigrationsWeb :: WebMApp ()
doMigrationsWeb =
  getAndHead "/do_migrations" $
    TL.fromStrict . T.concat <$> showMigrations >>= text

latestSalesWeb :: WebMApp ()
latestSalesWeb =
  getAndHead "/latest_sales" $ go
  where
    go = text "sales"
    -- go = do
    --   sales <- getLatestRedshiftSales
    --   json sales


toSubmissionResult :: Text -> Either (S.SubmissionError S.HttpException BS.ByteString) a -> SubmissionResult
toSubmissionResult submissionId' res = SubmissionResult {
    submissionId = submissionId'
  , isValid = const False ||| const True $ res
  , keyword =  Nothing
  , errorText = Just . submissionErrorToText . S.toSubmissionErrorType ||| const Nothing $ res
  , errorType = (Just . S.toSubmissionErrorType ||| const Nothing $ res)
  }

submissionErrorToText :: IsString p => SubmissionErrorType -> p
submissionErrorToText S.SENetworkError      = "Network Error"
submissionErrorToText S.SEInvalidMSISDN   = "MSISDN Validation Failed"
submissionErrorToText S.SEInvalidPIN = "PIN Validation Failed"
submissionErrorToText S.SEAlreadySubscribed = "MSISDN is already subscribed"
submissionErrorToText S.SEExceededMSISDNSubmissions = "Exceeded MSISDN Submissions"
submissionErrorToText S.SEUnknownError = "Unknown Error"
submissionErrorToText S.SEKeywordAndShortcodeNotFound = "Keyword and Shortcode not found"
submissionErrorToText S.SEUnknownSystemError = "Unknown System Error"
submissionErrorToText S.SELandingPageNotFound = "Landing page not found"
submissionErrorToText S.SEUnableToParseHTML = "Unable to parse HTML"

toSubmissionResultForMOFlow :: Text -> Either (S.SubmissionError S.HttpException BS.ByteString) S.MOFlowSubmissionResult -> SubmissionResult
toSubmissionResultForMOFlow submissionId' res = SubmissionResult {
    submissionId = submissionId'
  , isValid = const False ||| const True $ res
  , keyword =  const Nothing ||| Just $ res
  , errorText = Just . submissionErrorToText . S.toSubmissionErrorType ||| const Nothing $ res
  , errorType = (Just . S.toSubmissionErrorType ||| const Nothing $ res)
  }

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

queryStringParams :: ActionT TL.Text WebM [(Text, Text)]
queryStringParams = parseEncodedParams . W.rawQueryString <$> request where
  parseEncodedParams :: BS.ByteString -> [(Text, Text)]
  parseEncodedParams bs =
    [ (k, fromMaybe "" v)
    | (k, v) <- parseQueryText bs
    ]

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
      appState <- lift ask
      exists <- liftIO $ msisdnExists appState country msisdn
      res <- if exists
              then return $ Left $ S.APIError S.AlreadySubscribed "AlreadySubscribed From Internal Cache"
              else liftIO $ S.runSubmission $ S.submitMSISDN (unpack domain) (unpack handle) (unpack country) offer (unpack msisdn) additionalParams'
      (sid :: Integer) <- fromIntegral . fromSqlKey <$> addMSISDNSubmission domain country handle offer msisdn additionalParams res
      let psid = pack . encrypt' . show $ sid
      addScotchHeader "SubmissionId" (TL.fromStrict psid)
      json $ toSubmissionResult psid res
    else do -- MOFlow
      appState <- lift ask
      exists <- liftIO $ msisdnExists appState country msisdn
      res <- if exists
              then return $ Left $ S.APIError S.AlreadySubscribed "AlreadySubscribed From Internal Cache"
              else liftIO $ S.runSubmission $ MO.submitMSISDN (unpack domain) (unpack handle) (unpack country) offer (unpack msisdn) additionalParams'
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
      (sid' :: Integer) <- fromIntegral . fromSqlKey <$> addPINSubmission sid pin (fmap fst res)
      let epsid =  encrypt' . show $ sid'
      addScotchHeader "SubmissionId" (TL.pack epsid)
      let extractedFinalUrl = either (const Nothing) id $ fmap (extractFinalUrl . snd) res
      json FinalResult { finalUrl = fromMaybe (finalUrl' country) extractedFinalUrl , finalSubmissionResult = toSubmissionResult (pack epsid) res }
    Nothing ->
      let err = TL.pack $ maybe "MSISDN Submission" (const "Final URL") submission' in
      status status500 >> text ("No " <> err <> " was Found for the Given sid: " <> TL.pack (show sid))
  where
    finalUrl' = finalUrl'' . toLower where
      finalUrl'' "gr" = "http://gr.mobiworldbiz.com/?uid=fdf098fcc6&uip=2.84.0.0"
      finalUrl'' "iq" = "http://iq.mobbfun.com/?uid=fdf098fcc6"
      finalUrl'' "ae" = "http://ae.mobbfun.com/?uid=YV1-AE-2"
      finalUrl'' "sa" = "http://sa.gamezones.biz/?uid=fdf098fcc6"
      finalUrl'' "bh" = "http://bh.game-lords.com/#/?uid=fdf098fcc6"
      finalUrl''  _   = "http://gr.mobiworldbiz.com/?uid=fdf098fcc6&uip=2.84.0.0"

    -- fmap ("https://de-mcb-api.sam-media.com" <>) .
    extractFinalUrl :: T.Text -> Maybe T.Text
    extractFinalUrl = ( fmap ("https://de-mcb-api.sam-media.com" <>) . (safeHead =<<) . fmap (T.splitOn "\"") . safeSecond)  . T.splitOn "https://de-mcb-api.sam-media.com"

    safeSecond (_:t:_) = Just t
    safeSecond _ = Nothing

    safeHead (t:_) = Just t
    safeHead _ = Nothing


msisdnExistsWeb :: WebMApp ()
msisdnExistsWeb =
  getAndHead "/check_msisdn_active_subscription/:country/" $ do
    msisdn' <- param "msisdn"
    country' <- param "country"
    res <- JM.runJewlDb $ JM.msisdnStatus country' msisdn'
    json res
