{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Sam.Robot
(
    submitMSISDN, submitPIN, runSubmission, C.HttpException (..), SubmissionError (..)
  , SubmissionErrorType, toSubmissionErrorType
  , main -- for demonstration purpose only
) where

import           Control.Exception          (try)
import           Control.Monad              (join)
import           Control.Monad.Except       (liftIO)
import qualified Control.Monad.Trans.Except as X
import qualified Data.Aeson                 as A
import qualified Data.ByteString            as BS
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import qualified Data.Text.IO               as T
import           Database.Persist.TH
import           GHC.Generics               (Generic)
import qualified Network.HTTP.Client        as C
import qualified Network.HTTP.Types.Header  as Header
import qualified Network.HTTP.Types.URI     as U
import qualified Network.URI                as U


type Submission e b = X.ExceptT (SubmissionError e b) IO
data SubmissionError e b = NetworkError e | ValidationError b | AlreadySubscribed b | ExceededMSISDNSubmissions b deriving Show

data SubmissionErrorType = SENetworkError | SEInvalidMSISDN | SEAlreadySubscribed | SEExceededMSISDNSubmissions deriving (Show, Read, Enum, Eq, Ord, Bounded, Generic, A.ToJSON, A.FromJSON)

toSubmissionErrorType :: SubmissionError e b -> SubmissionErrorType
toSubmissionErrorType (NetworkError      _)         = SENetworkError
toSubmissionErrorType (ValidationError _)           = SEInvalidMSISDN
toSubmissionErrorType (AlreadySubscribed _)         = SEAlreadySubscribed
toSubmissionErrorType (ExceededMSISDNSubmissions _) = SEExceededMSISDNSubmissions

derivePersistField "SubmissionErrorType"




runSubmission :: Submission e b a -> IO (Either (SubmissionError e b) a)
runSubmission = X.runExceptT

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
submitMSISDN' :: String -> String -> String -> Int -> String -> Submission C.HttpException b (U.URI, BS.ByteString)
submitMSISDN' domain handle country offer msisdn =
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
  where
  sanitize "iq" ('9':'6':'4':xs) = xs
  sanitize _ x                   = x

validateSubmission
  :: (BS.ByteString -> T.Text -> Maybe (SubmissionError C.HttpException BS.ByteString))
  -> Bool
  -> T.Text
  -> (b, BS.ByteString)
  -> Submission C.HttpException BS.ByteString b
validateSubmission customCheck includes search (url, bs) = case customCheck bs content of
  Just m -> X.throwE m
  Nothing -> if (`op` T.empty) $ snd $ T.breakOn search content
    then X.throwE (ValidationError bs)
    else return url
  where
    op = if includes then (==) else (/=)
    content = E.decodeUtf8 bs


isAlreadySubscribed :: T.Text -> Bool
isAlreadySubscribed = T.isInfixOf "Θα λάβεις τώρα τον προσωπικό"

isExceededMSISDNSubmissions :: T.Text -> Bool
isExceededMSISDNSubmissions = T.isInfixOf "لقد تجاوزت الحد"

validateMSISDNSubmission :: (b, BS.ByteString) -> Submission C.HttpException BS.ByteString b
validateMSISDNSubmission = validateSubmission
  ( \bs content -> if isAlreadySubscribed content
    then Just (AlreadySubscribed bs)
    else if isExceededMSISDNSubmissions content
      then Just (ExceededMSISDNSubmissions bs)
      else Nothing
  )
  True
  "numeric-field pin pin-input"


validatePINSubmission :: (b, BS.ByteString) -> Submission C.HttpException BS.ByteString b
validatePINSubmission = validateSubmission (\_ _ -> Nothing) False "numeric-field pin pin-input"

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

submitMSISDN :: String -> String -> String -> Int -> String -> X.ExceptT (SubmissionError C.HttpException BS.ByteString) IO U.URI
submitMSISDN d h c o = (validateMSISDNSubmission =<<) . submitMSISDN' d h c o

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
    url <- validateMSISDNSubmission =<< submitMSISDN' "n.mobfun.co" "mobile-arts" "iq" 841 msisdn
    liftIO $ putStrLn "MSISDN Submission Successful"
    pin <- liftIO $ do
      putStrLn "PIN?"
      readLn
    finalUrl <- validatePINSubmission =<< submitPIN' pin url
    liftIO $ putStrLn "PIN Submission Successful"
    liftIO $ print finalUrl
    return finalUrl
  case finalResult of
    Left (NetworkError e) -> putStrLn "NetworkError" >> print e
    Left (ValidationError bs) -> putStrLn "Validation Error" >> T.writeFile "/Users/homam/temp/submissoinf.html" (E.decodeUtf8 bs)
    Left (AlreadySubscribed bs) -> putStrLn "AlreadySubscribed Error" >> T.writeFile "/Users/homam/temp/submissoinf.html" (E.decodeUtf8 bs)
    Left (ExceededMSISDNSubmissions bs) -> putStrLn "ExceededMSISDNSubmissions Error" >> T.writeFile "/Users/homam/temp/submissoinf.html" (E.decodeUtf8 bs)
    Right url -> putStrLn "Success" >> print url
