{-# LANGUAGE OverloadedStrings #-}

import TwitterMonad.Types
import TwitterMonad.OptionParser
import qualified Data.ByteString.Char8 as B

import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans (lift, MonadIO, liftIO)
import Control.Monad (void, when)
import System.Environment (getArgs)

import TwitterMonad.Twitter
import TwitterMonad.Conf
import System.IO (IOMode(..), hClose, hGetLine, hPutStrLn, withFile)
import System.Directory (doesFileExist, getHomeDirectory)

confFile :: IO FilePath
confFile = (++ "/.twittermonad") <$> getHomeDirectory

authenticateCLI :: IO ()
authenticateCLI = do
    liftIO $ putStrLn "request oauth token ..."
    Right (token, tokensec) <- runTwitter defaultConf authenticate
    file <- confFile
    liftIO $ withFile file WriteMode $ \h -> do
        B.hPut h token
        hPutStrLn h ""
        B.hPut h tokensec
        hPutStrLn h ""
    liftIO $ putStrLn "OK"

exeTwitter :: Conf -> Twitter a -> IO ()
exeTwitter c t = do
    result <- runTwitter c t
    case result of
        Right _ -> return ()
        Left x -> putStrLn x

main :: IO ()
main = do
    confFileExists <- confFile >>= doesFileExist
    when (not confFileExists) authenticateCLI

    file <- confFile
    (token, tokensec) <- withFile file ReadMode $ \h -> do
        token <- B.pack <$> hGetLine h
        tokensec <- B.pack <$> hGetLine h
        return (token, tokensec)
    let conf = defaultConf { authToken = token
                           , authTokenSecret = tokensec
                           }
    exeTwitter conf $ do
        opt <- liftIO getArgs
        case parseOption opt of
            OptHomeTimeline -> homeTimeline >>= liftIO . putTweets
            OptTweet tx -> tweet tx >> (liftIO . putStrLn $ "OK")
            OptOAuthToken -> liftIO authenticateCLI

            OptHelp msg -> liftIO . putStrLn $ msg
            OptUnknown -> liftIO . putStrLn $ "unknown"

        where
            putTweets :: [Tweet] -> IO ()
            putTweets = mapM_ (\(Tweet {text=text, user=User{screen_name=sname}}) -> putStrLn $ "[@" ++ sname ++ "]" ++ text)
