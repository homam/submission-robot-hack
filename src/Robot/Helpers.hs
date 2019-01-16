{-# LANGUAGE OverloadedStrings #-}

module Robot.Helpers where

import           Control.Exception          (try)
import           Control.Monad              (join)
import           Control.Monad.Except       (liftIO)
import qualified Control.Monad.Trans.Except as X
import qualified Data.Aeson                       as A
import qualified Network.HTTP.Client        as C
import qualified Network.HTTP.Types.URI     as U
import qualified Network.URI                as U
import           Robot.Types                hiding (handle)
import qualified Data.ByteString            as BS
import qualified Network.HTTP.Types.Header  as Header
import qualified Data.Text as T
import           GHC.Generics                         (Generic)

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

sanitize "iq" ('9':'6':'4':xs) = xs
sanitize "gr" ('3':'0':xs)     = xs
sanitize "mx" ('5':'2':xs)     = xs
sanitize "sk" ('4':'2':'1':xs) = xs
sanitize "ee" ('3':'7':'2':xs) = xs
sanitize "sa" ('9':'6':'6':xs) = '0':xs
sanitize "bh" ('9':'7':'3':xs) = xs
sanitize "my" ('6':xs)         = xs
sanitize "ae" ('9':'7':'1':xs) = '0':xs
sanitize "ae" ('0':xs) = '0':xs
sanitize "ae" xs = '0':xs
sanitize "gh" ('0':xs) = '0':xs
sanitize "gh" ('2':'3':'3':xs) = '0':xs
sanitize _ x                   = x