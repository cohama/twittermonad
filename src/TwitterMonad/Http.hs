{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, OverloadedStrings #-}

module TwitterMonad.Http
( HttpMethod (..)
, doRequest
) where

import Control.Arrow (first)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import Network.Wreq (Response, FormParam(..), defaults, getWith, postWith, responseBody, params, header)
import qualified Network.Wreq as Wreq
import Control.Lens ((&), (.~), (^.))

import qualified TwitterMonad.OAuth as OAuth

data HttpMethod = Get
                | Post
                deriving (Eq)

instance Show HttpMethod where
    show Get = "GET"
    show Post = "POST"

doRequest :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> String -> HttpMethod -> String -> [(String, Text)] -> IO (Response LBS.ByteString)
doRequest ckey csec token secret apiBase httpMethod path params = do
    let url = apiBase ++ path
    authHeader <- OAuth.makeAuthorization (show httpMethod) url params ckey csec token secret
    let params' = (first Text.pack) <$> params
        opts = defaults & header "Accept" .~ ["*/*"]
                        & header "Connection" .~ ["close"]
                        & header "Authorization" .~ [authHeader]
    case httpMethod of
        Get ->
            let opts' = opts & Wreq.params .~ params'
            in
            Wreq.getWith opts' url
        Post ->
            let params'' = map (uncurry (:=) . first pack) params
            in
            postWith opts url params''
