{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Robot.PG
where

import           Control.Applicative
import           Control.Concurrent         (threadDelay)
import           Control.Exception          (try)
import           Control.Monad              (join)
import           Control.Monad.Except       (liftIO)
import           Control.Monad.State        as S
import qualified Control.Monad.Trans.Except as X
import qualified Data.ByteString            as BS
import qualified Data.Char                  as Ch
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import qualified Data.Text.IO               as T
import qualified Data.Time                  as Time
import           Debug.Trace
import qualified Haquery                    as HQ
import qualified Network.HTTP.Client        as C
import qualified Network.HTTP.Client.TLS    as CTLS
import qualified Network.HTTP.Types         as H
import qualified Network.HTTP.Types.Header  as Header
import qualified Network.HTTP.Types.URI     as U
import qualified Network.URI                as U
import           Robot.Types
import           System.Random              (randomRIO)

callPG :: H.Method -> Maybe BS.ByteString -> String -> String -> String
  -> Submission C.HttpException b (U.URI, BS.ByteString, BS.ByteString)
callPG method cookieStr requestBody userAgent url = do
  liftIO $ putStrLn url
  now <- liftIO Time.getCurrentTime
  x <-
    liftIO
    $   try
    $   join
    $   C.withResponseHistory
    <$> fmap
          (\req -> req
            { C.requestHeaders =
              [(Header.hUserAgent, E.encodeUtf8 $ T.pack userAgent)]
                ++ (if method == "POST" then [(Header.hContentType, "application/x-www-form-urlencoded")] else []) -- C.requestHeaders req : () ,
                ++ (maybe [] (\s -> [("Cookie", s)]) cookieStr)
                ++ [("Referer", "http://wap.trend-tech.net/landings/mobilicerik/love-test-exclusive")]
              , C.cookieJar = maybe (Just $ C.createCookieJar []) (const Nothing) cookieStr -- C.cookieJar req <|> Just cookieJar-- Just $ C.createCookieJar $ C.destroyCookieJar (C.cookieJar req) ++ C.destroyCookieJar cookieJar,
              , C.method = method
              , C.requestBody = C.RequestBodyBS (E.encodeUtf8 $ T.pack requestBody)
            }
          )
          (C.parseRequest url)
    <*> CTLS.newTlsManagerWith C.defaultManagerSettings
    <*> return
          (\hr -> do
            let finalResponse = C.hrFinalResponse hr
            let cookieStr' = fst $ C.computeCookieString (C.hrFinalRequest hr) (C.responseCookieJar finalResponse) now True
            b <- BS.concat <$> C.brConsume (C.responseBody finalResponse)
            return (C.getUri $ C.hrFinalRequest hr, b, cookieStr')

          )
  case x of
    Left  e -> X.throwE (NetworkError e)
    Right r -> return r

genSKey :: String -> String
genSKey serviceKey =
  gen 1 (reverse serviceKey) 0 "" where

  gen :: Int -> String -> Int -> String -> String
  gen height str y ret
    | y == length str = take 36 ret
    | otherwise = do
        let topIn = fromEnum (str !! y)
        let secs = current !! y : current !! ((y + topIn + height) `mod` 36) : current !! ((y + 5 + topIn + height) `mod` 36) : []
        let secs' = if y `mod` 2 == 0
                      then map Ch.toLower secs
                      else secs
        gen height str (y + 1) (ret ++ secs')

  current :: [Char]
  current = [
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'Q', 'W', 'X', 'T', 'U', 'V', 'Y', 'Z',
        '$', '#', '+', '-', '?', ':', '.', ';', '_', '~'];

submitMSISDN ::
     String
  -> [(String, String)]
  -> Submission
       C.HttpException
       BS.ByteString
       BS.ByteString
submitMSISDN msisdn additionalParams = do
  let url = "http://n.mobfun.me/tr/football-goal?offer=1"
  let
    userAgent
      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36"
  (_, bs, cookieStr) <- callPG "GET" Nothing "" userAgent url

  case maybeToEither "servicekey not found" . firstRightJust . map (attrValues "value" "#servicekey") =<< (toHTML bs) of
    Left e -> X.throwE (APIError LandingPageNotFound bs)
    Right servicekey -> do
      let skey = genSKey (T.unpack servicekey)
      liftIO $ do
          print servicekey
          print skey
          threadDelay =<< (* 1000) <$> randomRIO (100, 1000)
      (url', bs', _) <- submitMsisdn (Just cookieStr) userAgent msisdn (T.unpack servicekey) skey "1" "" -- ccount == height
      liftIO $ print url'
      liftIO $ T.writeFile "/tmp/submissoinf.html" (E.decodeUtf8 bs')

      html <- toHTMLM bs'
      if hasElementL "#otp" html
        then return cookieStr
        else do
            liftIO $ print "otp not found"
            X.throwE (APIError LandingPageNotFound bs)


main :: IO ()
main = do
  let url = "http://n.mobfun.me/tr/football-goal?offer=1"
  let
    userAgent
      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36"

  -- use 5127764646
  msisdn <- getLine
  eRes <- runSubmission $ do
      -- (url, bs, cookieStr) <- callPG "GET" Nothing "" userAgent url
      -- liftIO $ do
      --     liftIO $ T.writeFile "/tmp/1.html" (E.decodeUtf8 bs)
      --     print url
      -- let content = E.decodeUtf8 bs
      -- let html = HQ.parseHtml content
      -- case maybeToEither "servicekey not found" . firstRightJust . map (attrValues "value" "#servicekey") =<< html of
      --   Left e -> liftIO $ print e
      --   Right servicekey -> do
      --     let skey = genSKey (T.unpack servicekey)
      --     liftIO $ do
      --         print servicekey
      --         print skey
      --     (_, bs', cookieStr') <- submitMsisdn (Just cookieStr) userAgent msisdn (T.unpack servicekey) skey "1" "" -- ccount == height
      --     liftIO $ T.writeFile "/tmp/submissoinf.html" (E.decodeUtf8 bs')
      --     liftIO $ BS.putStrLn bs'

      cookieStr <- submitMSISDN msisdn []

      pin <- liftIO getLine
      (_, bs'', _) <- submitPin (Just cookieStr) userAgent pin -- "2761"
      liftIO $ T.writeFile "/tmp/pin.html" (E.decodeUtf8 bs'')
      liftIO $ BS.putStrLn bs''


      return ()

  case eRes of
    Left e  -> print e
    Right r -> print r


submitMsisdn cookieStr userAgent msisdn servicekey skey ccount ckey =
  callPG
    "POST"
    cookieStr
    ("msisdn=" ++ msisdn ++ "&servicekey=" ++ servicekey ++ "&ccount=1&ckey=&skey=" ++ skey)
    userAgent -- "http://nghttp2.org/httpbin/post" --
    "http://wap.trend-tech.net/landings/subscribe" -- "http://wap.trend-tech.net/landings/confirm-otp"


submitPin cookieStr userAgent pin =
  callPG
    "POST"
    cookieStr
    ("otp=" ++ pin)
    userAgent -- "http://nghttp2.org/httpbin/post" --
    "http://wap.trend-tech.net/landings/confirm-otp"


validateMSISDNSubmission
  :: (U.URI, BS.ByteString, BS.ByteString) -> Submission C.HttpException BS.ByteString U.URI
validateMSISDNSubmission (url, bs, cookieStr) = error ""
  -- [("errorMsg", "Hattınız mobil ödeme servisine kapalıdır.")]




inPinPage, isNumberEntryPage :: [HQ.Tag] -> Bool
inPinPage = hasElementL "#top"
isNumberEntryPage = hasElementL "msisdn"


