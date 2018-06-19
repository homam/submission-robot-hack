{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Robot.Types where

import           Control.Monad              (join)
import qualified Control.Monad.Trans.Except as X
import qualified Data.Aeson                 as A
import qualified Data.Text                  as T
import           Database.Persist.TH
import           GHC.Generics               (Generic)
import qualified Haquery                    as HQ


type Submission e b = X.ExceptT (SubmissionError e b) IO
data SubmissionError e b = NetworkError e | APIError APIErrorType b deriving (Show)

data MOFlowSubmissionResult = MOFlowSubmissionResult {  keyword :: T.Text, shortcode :: T.Text } deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON)

data APIErrorType = InvalidMSISDN | InvalidPIN | AlreadySubscribed | ExceededMSISDNSubmissions | UnknownError | KeywordAndShortcodeNotFound deriving Show

data SubmissionErrorType = SENetworkError | SEInvalidMSISDN | SEInvalidPIN | SEAlreadySubscribed | SEExceededMSISDNSubmissions | SEUnknownError | SEKeywordAndShortcodeNotFound deriving (Show, Read, Enum, Eq, Ord, Bounded, Generic, A.ToJSON, A.FromJSON)
derivePersistField "SubmissionErrorType"

toSubmissionErrorType :: SubmissionError e b -> SubmissionErrorType
toSubmissionErrorType (NetworkError _              ) = SENetworkError
toSubmissionErrorType (APIError InvalidMSISDN     _) = SEInvalidMSISDN
toSubmissionErrorType (APIError InvalidPIN        _) = SEInvalidPIN
toSubmissionErrorType (APIError AlreadySubscribed _) = SEAlreadySubscribed
toSubmissionErrorType (APIError ExceededMSISDNSubmissions _) =
  SEExceededMSISDNSubmissions
toSubmissionErrorType (APIError KeywordAndShortcodeNotFound _) =
  SEKeywordAndShortcodeNotFound
toSubmissionErrorType (APIError UnknownError _) = SEUnknownError



runSubmission :: Submission e b a -> IO (Either (SubmissionError e b) a)
runSubmission = X.runExceptT

contains :: T.Text -> T.Text -> Bool
contains s = (/= T.empty) . snd . T.breakOn s

innerText :: T.Text -> Either String [HQ.Tag] -> Either String T.Text
innerText selector = fmap T.concat . innerTexts selector

innerTexts :: T.Text -> Either String [HQ.Tag] -> Either String [T.Text]
innerTexts selector html =
  fmap join
    .   sequence
    <$> map (fmap (map HQ.innerText) . HQ.select selector)
    =<< html

