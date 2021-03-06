{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass         #-}

module Robot.SamMOXHR (submitMSISDN) 

where

import           Control.Exception          (try)
import           Control.Monad              (join)
import           Control.Monad.Except       (liftIO)
import qualified Control.Monad.Trans.Except as X
import qualified Data.Aeson                       as A
import qualified Network.HTTP.Client        as C
import           Data.List                  (intercalate)
import qualified Network.HTTP.Types.URI     as U
import qualified Network.URI                as U
import           Robot.Types                hiding (handle)
import           Data.Monoid                ((<>))
import qualified Data.ByteString            as BS
import qualified Network.HTTP.Types.Header  as Header
import qualified Data.Text as T
import           GHC.Generics                         (Generic)
import Robot.Helpers (callSAM, sanitize)

-- http://m.gamezones.biz/my/api-handle?country=my&handle=api-handle&offer=1&device=smart&isAjax=1&msisdn%5B0%5D=01123809094&msisdnSubmitted=Y

data MOXHRSamAPIResult = MOXHRSamAPIResult {
    status :: Bool
  , smsBody :: T.Text
  , error :: Maybe T.Text
} deriving (Show, Read, Eq, Generic, A.ToJSON, A.FromJSON)

toMOFlowSubmissoinResult :: T.Text -> Maybe MOFlowSubmissionResult
toMOFlowSubmissoinResult = split
  where
    split s = case T.split (== '?') s of
      [sms,body] -> MOFlowSubmissionResult <$> splitBody body <*> splitSMS sms
      _          -> Nothing
    splitSMS s = case T.split (==':') s of
      [_sms, shortcode] -> Just shortcode
      _                -> Nothing
    splitBody s = case T.split (=='=') s of
      [_body, keyword] -> Just keyword
      _               -> Nothing

-- http://m.gamezones.biz/my/battleship?device=smart&offer=1&isAjax=1&msisdn%5B0%5D=0174403225&msisdnSubmitted=Y

-- curl "http://w1.mozzi.com/gh/api-handle?country=gh&handle=secure-pro&offer=1&device=smart&isAjax=1&msisdn%5B0%5D=0543959331&msisdnSubmitted=Y"
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
    "&msisdnSubmitted=Y&device=smart&isAjax=1&msisdn%5B0%5D="
    <> sanitize country msisdn
    <> "&" <> intercalate "&" (map (\ (k, v) -> k <> "=" <> v)  additionalParams)

submitMSISDN :: String -> String -> String -> Int -> String -> [(String, String)] -> Submission C.HttpException BS.ByteString MOFlowSubmissionResult
submitMSISDN domain handle country offer msisdn additionalParams = do
  (_url, bs) <- submitMSISDN' domain handle country offer msisdn additionalParams
  case A.decodeStrict bs of
    Nothing -> X.throwE $ APIError UnknownError bs
    Just (MOXHRSamAPIResult status body _error) ->
      if not status 
        then X.throwE $ APIError InvalidMSISDN bs
        else 
          let res' = toMOFlowSubmissoinResult body
          in case res' of 
            Nothing -> X.throwE $ APIError UnknownError bs -- unable to parse body
            Just res'' -> return res''

-- w1.mozzi.com/gh/api-handl
main = do
  finalResult <- X.runExceptT  $ submitMSISDN  "w1.mozzi.com" "api-handle" "gh" 1 "0543959335" []-- "m.gamezones.biz" "api-handle" "my" 1 "01123809094" []
  case finalResult of
    Left (NetworkError bs) -> putStrLn "NetworkError" >> print bs
    Left (APIError e bs) -> print e 
    Right res -> print res
    -- Right (url, bs) -> putStrLn "Success" >> print bs >> print ((A.decodeStrict bs) :: Maybe MOXHRSamAPIResult)
