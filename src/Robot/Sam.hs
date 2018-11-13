{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Robot.Sam
(
    submitMSISDN, submitMSISDNForMOFlow, submitPIN, runSubmission, C.HttpException (..), SubmissionError (..), APIErrorType (..), MOFlowSubmissionResult (..)
  , SubmissionErrorType (..), toSubmissionErrorType
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

callSAM :: String -> Submission C.HttpException b (U.URI, BS.ByteString)
callSAM url = do
  liftIO $ putStrLn url
  x <- liftIO $ try $ join $ C.withResponseHistory
      <$> fmap (\ req -> req {C.requestHeaders = (Header.hUserAgent, "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36") : C.requestHeaders req }) (C.parseRequest url)
      <*> C.newManager C.defaultManagerSettings
      <*> return (\hr ->
        (BS.concat <$> C.brConsume (C.responseBody $ C.hrFinalResponse hr) ) >>= \b -> return (C.getUri $ C.hrFinalRequest hr, b)
      )
  case x of
    Left e  -> X.throwE (NetworkError e)
    Right r -> return r

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
  where
  sanitize "iq" ('9':'6':'4':xs) = xs
  sanitize "gr" ('3':'0':xs)     = xs
  sanitize "mx" ('5':'2':xs)     = xs
  sanitize "sk" ('4':'2':'1':xs) = xs
  sanitize "ee" ('3':'7':'2':xs) = xs
  sanitize "ae" ('9':'7':'1':xs) = '0':xs
  sanitize _ x                   = x

includeErrors :: IsString t => [(t, APIErrorType)]
includeErrors =
  [ ("لقد تجاوزت الحد"                                       , ExceededMSISDNSubmissions)
  , ("الرقم الذي ادخلته غير صحيح"                               , InvalidMSISDN)
  , ("Θα λάβεις τώρα τον προσωπικό"                     , AlreadySubscribed)
  , ("Ο αριθμός του κινητού σας δεν είναι σωστός"       , InvalidMSISDN)
  , ("Τώρα έλαβες ένα SMS"                              , AlreadySubscribed)
  , ("El numero que has introducido no es correcto"     , InvalidMSISDN)
  , ("Τώρα έλαβες ένα SMS με"                           , AlreadySubscribed)
  , ("Error del sistema. Por favor, inténtalo de nuevo.", UnknownSystemError)
  ]

validateMSISDNSubmission :: (U.URI, BS.ByteString) -> Submission C.HttpException BS.ByteString U.URI
validateMSISDNSubmission (url, bs)
  | hasPin content = return url
  | otherwise =
    if Just "mx.combate-extremo.com" == fmap U.uriRegName (U.uriAuthority url)
      then X.throwE $ APIError AlreadySubscribed bs
      else maybe (X.throwE $ APIError UnknownError bs) X.throwE err

  where
    err = either
            (const $ (`APIError` bs) <$> lookup' (`T.isInfixOf` content) includeErrors)
            (\ t -> (`APIError` E.encodeUtf8 t) <$> lookup' (`T.isInfixOf` t) includeErrors)
            errMsg
    content = E.decodeUtf8 bs
    html = HQ.parseHtml content
    errMsg = innerText ".errMsg" html

    safeHead []    = Nothing
    safeHead (x:_) = Just x

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
    -- trace (T.unpack content) $
    keywordAndShortCode = toKeywordaAndShortCode =<< innerTexts "main h3 em" html

    toKeywordaAndShortCode :: [T.Text] -> Either String (T.Text, T.Text)
    toKeywordaAndShortCode [] = Left "Keyword and Shortcode not found on the Page"
    toKeywordaAndShortCode (_:[]) = Left "Keyword or Shortcode not found on the Page"
    toKeywordaAndShortCode (a:b:_) = Right (a, b)

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

validatePINSubmission :: (b, BS.ByteString) -> Submission C.HttpException BS.ByteString b
validatePINSubmission (url, bs)
  | or $ sequence [ hasPin, isMSISDNEntryPage, isOfferExpiredPage ] content = X.throwE $ APIError InvalidPIN (either (const bs) E.encodeUtf8 errMsg)
  | otherwise = return url
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
       U.URI
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
submitMSISDNForMOFlow d h c o m additionalParams = do
  res <- submitMSISDN' d h c o m additionalParams
  validateMSISDNSubmissionForMOFlow res



submitPIN :: BS.ByteString -> U.URI -> X.ExceptT (SubmissionError C.HttpException BS.ByteString) IO U.URI
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
    url <- validateMSISDNSubmission =<< submitMSISDN' "n.mobfun.co" "mobile-arts" "iq" 841 msisdn []
    liftIO $ putStrLn "MSISDN Submission Successful"
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
