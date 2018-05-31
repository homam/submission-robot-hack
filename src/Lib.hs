module Lib (
   main
  ) where

import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import qualified Database.Redis     as R
import           Web.Visit
import qualified Web.WebM           as W


myApp :: W.WebMApp ()
myApp = doMigrationsWeb >> msisdnExistsWeb >> msisdnSubmissionWeb >> msisdnSubmissionWebForMOFlow >> pinSubmissionWeb

main :: Int -> String -> String -> IO ()
main port jewlDb db = W.runWebServer port
                                     R.defaultConnectInfo
                                     (E.encodeUtf8 $ T.pack jewlDb)
                                     (E.encodeUtf8 $ T.pack db)
                                     myApp

