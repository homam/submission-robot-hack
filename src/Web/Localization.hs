{-# LANGUAGE OverloadedStrings #-}

module Web.Localization (
    toLocalMSISDN
  , encrypt, encrypt'
  , decrypt, decrypt'
) where

import qualified Codec.Crypto.AES           as AES
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8      as C8
import qualified Data.CaseInsensitive       as CI
import qualified Data.List                  as L
import qualified Data.Time.Clock.POSIX      as POSIX
import           Numeric                    (showHex)

algorithm = AES.crypt' AES.CFB "abcdefghijl1ertg" "abcdefghijl1ertg"


main = do
  encrypted <- B64.encode . algorithm AES.Encrypt . C8.pack . (`showHex` "") . round . (*1000000) . fromRational . toRational <$> POSIX.getPOSIXTime
  let decrypted = C8.unpack . algorithm AES.Decrypt  <$> B64.decode encrypted
  print encrypted
  print decrypted

encrypt' :: String -> String
encrypt' = C8.unpack . encrypt . C8.pack

encrypt :: C8.ByteString -> C8.ByteString
encrypt = B64.encode . algorithm AES.Encrypt


decrypt :: C8.ByteString -> Either String C8.ByteString
decrypt = fmap (algorithm AES.Decrypt) . B64.decode

decrypt' :: C8.ByteString -> Either String String
decrypt' = fmap C8.unpack . decrypt

toLocalMSISDN :: String -> String -> String
toLocalMSISDN country m = case CI.mk country of
  "gr" -> if "30" `L.isPrefixOf` m then drop 2 m else m
  _    -> m
