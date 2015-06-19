{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, OverloadedStrings #-}

module TwitterMonad.Twitter
( Twitter
, runTwitter
, homeTimeline
, tweet
, authenticate
)
where

import Data.Text (Text, pack)
import Control.Monad (void)
import Control.Monad.Trans (lift, MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask, local)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import TwitterMonad.Types (Tweet, User, fromJSON)
import qualified TwitterMonad.Http as Http
import TwitterMonad.Http (HttpMethod(..))
import TwitterMonad.Conf (Conf (..))
import Control.Exception (SomeException, catch)
import Control.Lens ((^.))
import Data.Aeson.Lens (_String, key)
import Network.Wreq (responseBody, atto)
import Data.Attoparsec.ByteString.Char8 (isDigit, isAlpha_ascii, takeWhile)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as LA
import System.Process (system)
import System.IO (hFlush, stdout)

hoistExcept :: (Monad m) => Either e a -> ExceptT e m a
hoistExcept = ExceptT . return

class (Monad m) => MonadTwitter m where
    homeTimeline :: m [Tweet]
    tweet :: String -> m ()
    authenticate :: m (BS.ByteString, BS.ByteString)

newtype Twitter a = Twitter { unTwitter :: ExceptT String (ReaderT Conf IO) a }
                    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Conf)


runTwitter :: Conf -> Twitter a -> IO (Either String a)
runTwitter c t = flip runReaderT c . runExceptT . unTwitter $ t

tryOr :: IO a -> (SomeException -> e) -> IO (Either e a)
tryOr action onerr = (Right <$> action) `catch` (return . Left . onerr)


api :: Http.HttpMethod -> String -> [(String, Text)] -> Twitter LBS.ByteString
api method url params = do
    ckey <- authConsumerKey <$> ask
    csec <- authConsumerSecret <$> ask
    tok <- authToken <$> ask
    toksec <- authTokenSecret <$> ask
    ret <- liftIO $ Http.doRequest ckey csec tok toksec
                                   "https://api.twitter.com/1.1/"
                                   method url params
                                   `tryOr` show
    ret' <- Twitter $ hoistExcept ret
    return $ ret' ^. responseBody

instance MonadTwitter Twitter where
    homeTimeline = do
        conf <- ask
        let count = pack . show . homeTimelineDefaultCount $ conf
        ts <- api Get "statuses/home_timeline.json" [("count", count)]
        Twitter . hoistExcept . fromJSON $ ts

    tweet t = void $ api Post "statuses/update.json" [("status", pack t)]

    authenticate = do
        ckey <- authConsumerKey <$> ask
        csec <- authConsumerSecret <$> ask
        res <- liftIO $ Http.doRequest ckey csec "" ""
                                       "https://api.twitter.com/"
                                       Post "oauth/request_token"
                                       [("oauth_callback", "oob")]
        let body = res ^. responseBody
            parse_params key = (A.string $ key `BS.snoc` '=')
                               *> A.takeWhile (orf [(=='-'), isAlpha_ascii, isDigit])
            parse_body = (,) <$> parse_params "oauth_token"
                             <*> ("&" *> parse_params "oauth_token_secret")
            result = LA.parse parse_body $ body
        case result of
            Done _ (token, tokensec) -> do
                let url = "https://api.twitter.com/oauth/authenticate?oauth_token=" ++ show token
                liftIO $ putStrLn $ "Opening " ++ url
                liftIO $ system $ "xdg-open \"" ++ url ++ "\" > /dev/null 2>&1"
                liftIO $ putStr "Input PIN > "
                liftIO $ hFlush stdout
                pin <- liftIO $ getLine
                ckey <- authConsumerKey <$> ask
                csec <- authConsumerSecret <$> ask
                res' <- liftIO $ Http.doRequest ckey csec token tokensec
                                               "https://api.twitter.com/"
                                               Post "oauth/access_token"
                                               [("oauth_verifier", pack pin)]
                let body' = res' ^. responseBody
                case LA.parse parse_body $ body' of
                    Done _ x -> return x
                    _ -> undefined

            _ -> undefined
        -- Twitter . hoistExcept $ show <$> result
            where
                orf :: [a -> Bool] -> a -> Bool
                orf xs c = or $ xs <*> [c]
