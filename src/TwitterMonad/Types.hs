{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module TwitterMonad.Types
( UserID
, User (..)
, TweetID
, Tweet (..)
, fromJSON
)
where

import GHC.Generics
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString.Lazy as LBS


newtype UserID = UserID Integer deriving (Show, Generic)

instance Aeson.FromJSON UserID

data User = User { name :: String
                 , usrid :: UserID
                 , screen_name :: String
                 , followers_count :: Int
                 , following :: Bool
                 }
                 deriving (Show)

instance Aeson.FromJSON User where
    parseJSON (Aeson.Object v) = User <$>
                                 v .: "name" <*>
                                 v .: "id" <*>
                                 v .: "screen_name" <*>
                                 v .: "followers_count" <*>
                                 v .: "following"

newtype TweetID = TweetID Integer deriving (Show, Generic)

instance Aeson.FromJSON TweetID

data Tweet = Tweet { created_at :: String
                   , id :: TweetID
                   , text :: String
                   , retweet_count :: Int
                   , retweeted :: Bool
                   , user :: User
                   }
                   deriving (Show, Generic)

instance Aeson.FromJSON Tweet

fromJSON :: Aeson.FromJSON a => LBS.ByteString -> Either String a
fromJSON a = case Aeson.decode a of
    Just x -> Right x
    Nothing -> Left "JSON parse error."
