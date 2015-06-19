module TwitterMonad.OptionParser
( parseOption
, Option (..)
)
where

data Option = OptHomeTimeline
            | OptTweet String
            | OptHelp String
            | OptOAuthToken
            | OptUnknown

parseOption :: [String] -> Option
parseOption [] = OptHelp $ "twittermonad tl\n\
                           \twittermonad tw \"text\"\n\
                           \twittermonad oauth\
                           \twittermonad help"
parseOption ("tl":_) = OptHomeTimeline
parseOption ("tw":s:_) = OptTweet s
parseOption ["oauth"] = OptOAuthToken
parseOption _ = OptUnknown
