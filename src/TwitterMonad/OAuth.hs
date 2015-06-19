{-# LANGUAGE OverloadedStrings #-}

module TwitterMonad.OAuth
( makeAuthorization
) where

import Control.Monad.Trans.Reader (Reader, runReader, ask, asks)
import Control.Arrow ((***), second)
import Data.ByteString.Base64.Lazy (encode)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.HTTP.Types as HttpTypes
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.List (intercalate, sort)

signatureMethod :: BS.ByteString
signatureMethod = "HMAC-SHA1"

hmacSha1WithBase64 :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmacSha1WithBase64 key msg = (LBS.toStrict . encode . SHA.bytestringDigest) $ SHA.hmacSha1 (LBS.fromStrict key) (LBS.fromStrict msg)

getNonce :: IO BS.ByteString
getNonce = (BS.pack . formatTime defaultTimeLocale "%s%q") <$> getCurrentTime

getTimestamp :: IO BS.ByteString
getTimestamp = (BS.pack . formatTime defaultTimeLocale "%s") <$> getCurrentTime

version :: BS.ByteString
version = "1.0"

urlEncodeBS :: BS.ByteString -> BS.ByteString
urlEncodeBS = HttpTypes.urlEncode True

urlEncodeT :: Text -> BS.ByteString
urlEncodeT = HttpTypes.urlEncode True . encodeUtf8

urlEncode :: String -> BS.ByteString
urlEncode = urlEncodeBS . BS.pack

createParameter :: Reader AuthParams BS.ByteString
createParameter = do
    consumerKey <- asks authConsumerKey
    nonce <- asks authNonce
    timestamp <- asks authTimestamp
    token <- asks authToken
    reqParams <- asks authRequestParams
    let params = map (second decodeUtf8)
                    [("oauth_consumer_key", consumerKey)
                   , ("oauth_nonce", nonce)
                   , ("oauth_signature_method", signatureMethod)
                   , ("oauth_timestamp", timestamp)
                   , ("oauth_token", token)
                   , ("oauth_version", version)
                   ] ++ reqParams
        sorted = sort . fmap (urlEncode *** urlEncodeT) $ params
    return . BS.intercalate "&" . map (\(k, v) -> k `BS.append` "=" `BS.append` v) $ sorted

tap :: IO String -> IO String
tap x = do
  x >>= putStrLn
  x

createSignatureBaseString :: Reader AuthParams BS.ByteString
createSignatureBaseString =
    BS.intercalate "&" . map urlEncodeBS <$> sequence [BS.pack <$> asks authHttpMethod, BS.pack <$> asks authUrl, createParameter]

signingKey :: Reader AuthParams BS.ByteString
signingKey =
    BS.intercalate "&" . map urlEncodeBS <$> sequence [asks authConsumerSecret, asks authTokenSecret]

createSignature :: Reader AuthParams BS.ByteString
createSignature = hmacSha1WithBase64 <$> signingKey <*> createSignatureBaseString

surround :: Char -> BS.ByteString -> BS.ByteString
surround c s = c `BS.cons` (s `BS.snoc` c)

data AuthParams = AuthParams { authHttpMethod :: String
                             , authUrl :: String
                             , authRequestParams :: [(String, Text)]
                             , authNonce :: BS.ByteString
                             , authTimestamp :: BS.ByteString
                             , authConsumerKey :: BS.ByteString
                             , authConsumerSecret :: BS.ByteString
                             , authToken :: BS.ByteString
                             , authTokenSecret :: BS.ByteString
                             }

makeAuthorization :: String -> String -> [(String, Text)] -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO BS.ByteString
makeAuthorization httpMethod baseUrl params conskey conssec token tokenSecret = do
    timestamp <- getTimestamp
    nonce <- getNonce
    let signature = runReader createSignature $ AuthParams { authHttpMethod     = httpMethod
                                                           , authUrl            = baseUrl
                                                           , authRequestParams  = params
                                                           , authNonce          = nonce
                                                           , authTimestamp      = timestamp
                                                           , authConsumerKey    = conskey
                                                           , authConsumerSecret = conssec
                                                           , authToken          = token
                                                           , authTokenSecret    = tokenSecret
                                                           }
        params' = [("oauth_consumer_key", conskey)
                 , ("oauth_nonce", nonce)
                 , ("oauth_signature", signature)
                 , ("oauth_signature_method", signatureMethod)
                 , ("oauth_timestamp", timestamp)
                 , ("oauth_token", token)
                 , ("oauth_version", version)
                 ]
    return . BS.append "OAuth " . BS.intercalate ", " . map (\(k, v) -> urlEncodeBS k `BS.append` "=" `BS.append` surround '"' (urlEncodeBS v)) $ params'
