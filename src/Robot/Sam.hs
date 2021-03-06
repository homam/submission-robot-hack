{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Robot.Sam
(
    submitMSISDN, submitMSISDNForMOFlow, submitPIN, runSubmission, C.HttpException (..), SubmissionError (..), APIErrorType (..), MOFlowSubmissionResult (..)
  , SubmissionErrorType (..), toSubmissionErrorType, getAlradySubscribedUrl
  , main -- for demonstration purpose only
) where

import           Control.Exception          (try)
import           Control.Monad              (join)
import           Control.Monad.Except       (liftIO)
import qualified Control.Monad.Trans.Except as X
import qualified Data.ByteString            as BS
import           Data.List                  (intercalate)
import           Data.Monoid                ((<>))
import           Data.String
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import qualified Data.Text.IO               as T
-- import           Debug.Trace                (trace)
import qualified Haquery                    as HQ
import qualified Network.HTTP.Client        as C
import qualified Network.HTTP.Types.Header  as Header
import qualified Network.HTTP.Types.URI     as U
import qualified Network.URI                as U
import           Robot.Types                hiding (handle)
import Robot.Helpers (callSAM, sanitize)
import qualified Robot.SamMOXHR as MO
import Control.Arrow

-- http://n.mobfun.co/iq/mobile-arts?country=iq&handle=mobile-arts&offer=841&msisdnSubmitted=Y&msisdn%5B0%5D=7814237252&legalCheckbox=Y&incentivizedCheckbox=Y&op_confirmCheckbox=N&identified=1
submitMSISDN' :: String -> String -> String -> Int -> String -> [(String, String)] -> Submission C.HttpException b (U.URI, BS.ByteString)
submitMSISDN' domain handle country offer msisdn additionalParams =
  callSAM $ "http://" <> domain <> "/" <> country <> "/" <> handle <>
    "?country="
    <> country
    <> "&handle="
    <> handle
    <> "&offer="
    <> show offer
    <>
    "&smart=1&identified=1&msisdnSubmitted=Y&incentivizedCheckbox=Y&legalCheckbox=Y&op_confirmCheckbox=N&msisdn%5B0%5D="
    <> sanitize country msisdn
    <> "&" <> intercalate "&" (map (\ (k, v) -> k <> "=" <> v)  additionalParams)

includeErrors :: IsString t => [(t, APIErrorType)]
includeErrors =
  [ ("لقد تجاوزت الحد"                                       , ExceededMSISDNSubmissions)
  , ("الرقم الذي ادخلته غير صحيح"                               , InvalidMSISDN)
  , ("Θα λάβεις τώρα τον προσωπικό"                     , AlreadySubscribed Nothing)
  , ("Ο αριθμός του κινητού σας δεν είναι σωστός"       , InvalidMSISDN)
  , ("Τώρα έλαβες ένα SMS"                              , AlreadySubscribed Nothing)
  , ("El numero que has introducido no es correcto"     , InvalidMSISDN)
  , ("Τώρα έλαβες ένα SMS με"                           , AlreadySubscribed Nothing)
  , ("Error del sistema. Por favor, inténtalo de nuevo.", UnknownSystemError)
  ]

validateMSISDNSubmission :: (U.URI, BS.ByteString) -> Submission C.HttpException BS.ByteString (U.URI, Maybe T.Text)
validateMSISDNSubmission (url, bs)
  | hasPin content = return (url, actualPIN)
  | otherwise =
    if contains "_extracted" content
      then maybe (X.throwE $ APIError UnknownError bs) X.throwE err
      else X.throwE $ APIError (AlreadySubscribed $ Just url) bs

  where
    err = either
            (const $ (`APIError` bs) <$> lookup' (`T.isInfixOf` content) includeErrors)
            (\ t -> (`APIError` E.encodeUtf8 t) <$> lookup' (`T.isInfixOf` t) includeErrors)
            errMsg
    content = E.decodeUtf8 bs
    html = HQ.parseHtml content
    errMsg = innerText ".errMsg" html
    actualPIN = either (const Nothing) id $ HQ.attr "value" <$> (safeHead' =<< HQ.select "input.pin.pin-input"  =<< safeHead' =<< html)

    safeHead []    = Nothing
    safeHead (x:_) = Just x

    safeHead' []    = Left "Empty List"
    safeHead' (x:_) = Right x

    lookup' :: (a -> Bool) -> [(a, b)] -> Maybe b
    lookup' p = fmap snd . safeHead . filter (p . fst)

validateMSISDNSubmissionForMOFlow :: (U.URI, BS.ByteString) -> Submission C.HttpException BS.ByteString MOFlowSubmissionResult
validateMSISDNSubmissionForMOFlow (_, bs)
  | hasKeyword content = proceed
  | otherwise =
        maybe (X.throwE $ APIError UnknownError bs) X.throwE $ -- maybe :: b -> (v -> b) -> Maybe v -> b  -- very similar to 'either' function below, 'maybe' converts Maybe v to b. Here b is actually: 'Submission C.HttpException BS.ByteString MOFlowSubmissionResult'
          either -- either :: (e -> b) -> (v -> b) -> Etiher e v -> b  -- takes an either and produces a new value of type b. Here b type is actually 'Maybe SubmissionError'
            (const $ (`APIError` bs) <$> lookup' (`T.isInfixOf` content) includeErrors)
            (\ t -> (`APIError` E.encodeUtf8 t) <$> lookup' (`T.isInfixOf` t) includeErrors)
            errMsg -- errMsg is of type Either (it can be either: Left someError | Right value )

  where
    proceed =
      let eks = keywordAndShortCode
      in case eks of
        Left _   -> X.throwE (APIError KeywordAndShortcodeNotFound bs)
        Right ks -> return $ uncurry MOFlowSubmissionResult ks

    content = E.decodeUtf8 bs
    html = HQ.parseHtml content

    errMsg :: Either String T.Text
    errMsg = innerText ".errMsg" html

    keywordAndShortCode :: Either String (T.Text, T.Text)
    keywordAndShortCode  = 
      let (sc, kw) = T.drop 1 . snd . T.breakOn ":" *** T.drop 1 . snd . T.breakOn "=" $ T.breakOn "?body=" $ fst . snd $ T.breakOn "\"" <$> T.breakOn "sms:" content
      in if T.length kw == 0 || T.length sc == 0 
        then Left "Keyword and Shortcode not found on the Page"
        else Right (kw, sc)


    -- keywordAndShortCode :: Either String (T.Text, T.Text)
    -- keywordAndShortCode = toKeywordaAndShortCode =<< innerTexts "main h3 em" html

    -- toKeywordaAndShortCode :: [T.Text] -> Either String (T.Text, T.Text)
    -- toKeywordaAndShortCode [] = Left "Keyword and Shortcode not found on the Page"
    -- toKeywordaAndShortCode (_:[]) = Left "Keyword or Shortcode not found on the Page"
    -- toKeywordaAndShortCode (a:b:_) = Right (a, b)

    safeHead []    = Nothing
    safeHead (x:_) = Just x

    lookup' :: (a -> Bool) -> [(a, b)] -> Maybe b
    lookup' p = fmap snd . safeHead . filter (p . fst)

hasKeyword :: T.Text -> Bool
hasKeyword = contains "keyword"

hasPin :: T.Text -> Bool
hasPin = contains "numeric-field pin pin-input"

isMSISDNEntryPage :: T.Text -> Bool
isMSISDNEntryPage = contains "numeric-field msisdn"

isOfferExpiredPage :: T.Text -> Bool
isOfferExpiredPage = contains "This offer has expired."

validatePINSubmission :: (b, BS.ByteString) -> Submission C.HttpException BS.ByteString (b, T.Text)
validatePINSubmission (url, bs)
  | or $ sequence [ hasPin, isMSISDNEntryPage, isOfferExpiredPage ] content = X.throwE $ APIError InvalidPIN (either (const bs) E.encodeUtf8 errMsg)
  | otherwise = return (url, content)
  where
    content = E.decodeUtf8 bs
    html = HQ.parseHtml content
    errMsg = innerText ".errMsg" html

submitPIN' :: BS.ByteString -> U.URI -> Submission C.HttpException b (U.URI, BS.ByteString)
submitPIN' pin url = callSAM $ (U.uriToString id $ makePINUrl pin url) ""

makePINUrl :: BS.ByteString -> U.URI -> U.URI
makePINUrl pin url = U.URI (U.uriScheme url) (U.uriAuthority url) (U.uriPath url) (queryString pin url) ""
  where
    queryString pin' url' = T.unpack $ E.decodeUtf8 $ U.renderQuery True $ [("pinSubmitted", Just "Y"), ("pin", Just pin')] ++ keepRelevantParams url'
    keepRelevantParams url' = filter ((`elem` [
      "country",
      "handle",
      "offer",
      "device",
      "msisdn[0]",
      "incentivizedCheckbox",
      "legalCheckbox",
      "identified",
      "operator",
      "msisdnSubmitted",
      "rid",
      "_extracted"
      ]) . fst) $ U.parseQuery $ E.encodeUtf8 $ T.pack $ U.uriQuery url'

submitMSISDN ::
     String
  -> String
  -> String
  -> Int
  -> String
  -> [(String, String)]
  -> Submission
       C.HttpException
       BS.ByteString
       (U.URI, Maybe T.Text)
submitMSISDN d h c o m additionalParams = do
  res <- submitMSISDN' d h c o m additionalParams
  validateMSISDNSubmission res


submitMSISDNForMOFlow ::
     String
  -> String
  -> String
  -> Int
  -> String
  -> [(String, String)]
  -> Submission
       C.HttpException
       BS.ByteString
       MOFlowSubmissionResult
-- submitMSISDNForMOFlow d h c o m additionalParams = MO.submitMSISDN d h c o m additionalParams
submitMSISDNForMOFlow d h c o m additionalParams = do
  res <- submitMSISDN' d h c o m additionalParams
  validateMSISDNSubmissionForMOFlow res



submitPIN :: BS.ByteString -> U.URI -> X.ExceptT (SubmissionError C.HttpException BS.ByteString) IO (U.URI, T.Text)
submitPIN p = (validatePINSubmission =<<) . submitPIN' p

-- 6949041021
-- 6949713057
-- | for demonstration purpose only
main :: IO ()
main = do
  putStrLn "Hello!"
  finalResult <- X.runExceptT $ do
    msisdn <- liftIO $ do
      putStrLn "MSISDN?"
      readLn
    (url, actualPIN) <- validateMSISDNSubmission =<< submitMSISDN' "n.mobfun.co" "mobile-arts" "iq" 841 msisdn []
    liftIO $ putStrLn "MSISDN Submission Successful"
    liftIO $ putStrLn $ "Actual PIN" ++ show actualPIN
    pin <- liftIO $ do
      putStrLn "PIN?"
      readLn
    finalUrl <- validatePINSubmission =<< submitPIN' pin url
    liftIO $ putStrLn "PIN Submission Successful"
    liftIO $ print finalUrl
    return finalUrl
  case finalResult of
    Left (NetworkError bs) -> putStrLn "NetworkError" >> print bs
    Left (APIError e bs) -> print e >> T.writeFile "/Users/homam/temp/submissoinf.html" (E.decodeUtf8 bs)
    Right url -> putStrLn "Success" >> print url
