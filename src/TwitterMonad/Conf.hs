{-# LANGUAGE OverloadedStrings #-}

module TwitterMonad.Conf
( Conf (..)
, defaultConf
)
where

import Data.ByteString.Char8 (ByteString)

data Conf = Conf { authConsumerKey :: ByteString
                 , authConsumerSecret :: ByteString
                 , authToken :: ByteString
                 , authTokenSecret :: ByteString
                 , homeTimelineDefaultCount :: Int
                 }
            deriving (Show, Read)

defaultConf :: Conf
defaultConf = Conf { authConsumerKey = "sXxt5IDOvpmnd8dsTZwPwhIpm"
                   , authConsumerSecret = "0hSDGEdkFuqoJjn1sQmlOZfCokIPJYAzaIXj8ugyKvjHYrTjkE"
                   , authToken = ""
                   , authTokenSecret = ""
                   , homeTimelineDefaultCount = 200
                   }
