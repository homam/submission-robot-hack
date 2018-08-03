{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Robot.Types where

import           Control.Monad              (join)
import qualified Control.Monad.Trans.Except as X
import qualified Data.Aeson                 as A
import qualified Data.ByteString            as BS
import           Data.Either
import qualified Data.List                  as L
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import           Database.Persist.TH
import           GHC.Generics               (Generic)
import qualified Haquery                    as HQ

type Submission e b = X.ExceptT (SubmissionError e b) IO
data SubmissionError e b = NetworkError e | APIError APIErrorType b deriving (Show)

data MOFlowSubmissionResult = MOFlowSubmissionResult {  keyword :: T.Text, shortcode :: T.Text } deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON)

data APIErrorType = InvalidMSISDN | InvalidPIN | AlreadySubscribed | ExceededMSISDNSubmissions | UnknownError | UnknownSystemError
                  | KeywordAndShortcodeNotFound | LandingPageNotFound | UnableToParseHTML
  deriving Show

data SubmissionErrorType = SENetworkError | SEInvalidMSISDN | SEInvalidPIN | SEAlreadySubscribed | SEExceededMSISDNSubmissions | SEUnknownError | SEUnknownSystemError
                         | SEKeywordAndShortcodeNotFound | SELandingPageNotFound | SEUnableToParseHTML

  deriving (Show, Read, Enum, Eq, Ord, Bounded, Generic, A.ToJSON, A.FromJSON)
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
toSubmissionErrorType (APIError LandingPageNotFound _) = SELandingPageNotFound
toSubmissionErrorType (APIError UnableToParseHTML _) = SEUnableToParseHTML
toSubmissionErrorType (APIError UnknownSystemError _) = SEUnknownSystemError


runSubmission :: Submission e b a -> IO (Either (SubmissionError e b) a)
runSubmission = X.runExceptT

contains :: T.Text -> T.Text -> Bool
contains s = (/= T.empty) . snd . T.breakOn s

innerText :: T.Text -> Either String [HQ.Tag] -> Either String T.Text
innerText selector = fmap T.concat . innerTexts selector

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

attrValue :: T.Text -> T.Text -> HQ.Tag -> Either String T.Text
attrValue attr selector html = maybeToEither "Attribute does not exist" =<< HQ.attr attr . head <$> HQ.select selector html

attrValues :: T.Text -> T.Text -> HQ.Tag -> Either String [Maybe T.Text]
attrValues attr selector h = do
  elements <- HQ.select selector h
  let attrs = filter isJust $ map (HQ.attr attr) elements
  return attrs

handle :: [Either e a] -> [a]
handle = map fromRight' . filter isRight where

fromRight' :: Either e a -> a
fromRight' (Right a) = a
fromRight' (Left _)  = error "Left"

firstRightJust :: [Either e [Maybe a]] -> Maybe a
firstRightJust = go . map fromJust . filter isJust . join .  map fromRight' . filter isRight where
  go :: [a] -> Maybe a
  go []    = Nothing
  go (x:_) = Just x


hasElement :: T.Text -> HQ.Tag -> Bool
hasElement selector = either (const False) (/= []) . HQ.select selector

hasElementL :: T.Text -> [HQ.Tag] -> Bool
hasElementL selector = any (hasElement selector)


innerTexts :: T.Text -> Either String [HQ.Tag] -> Either String [T.Text]
innerTexts selector html =
  fmap join
    .   sequence
    <$> map (fmap (map HQ.innerText) . HQ.select selector)
    =<< html

toHTML :: BS.ByteString -> Either String [HQ.Tag]
toHTML = HQ.parseHtml . E.decodeUtf8

toHTMLM :: BS.ByteString -> Submission e BS.ByteString [HQ.Tag]
toHTMLM bs = either (const $ X.throwE $ APIError UnableToParseHTML bs) return (toHTML bs)
