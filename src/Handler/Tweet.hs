{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Tweet where

import Import
import Data.Text

-- HELPERS

import Helpers.Application (applicationLayout, applicationNotLoggedLayout)

-- WEB

getTweetsPageR :: Text -> Handler Html
getTweetsPageR tweetuserident = do
    userid <- lookupSession "UserId"
    case userid of
        Just userid -> (tweetsPageUserLogged tweetuserident)
        Nothing -> (tweetsPageUserNotLogged tweetuserident)

tweetsPageUserLogged :: Text -> Handler Html
tweetsPageUserLogged tweetuserident = do
    tweetUser <- runDB $ selectFirst [UserIdent ==. tweetuserident] []
    applicationLayout $ do
        $(widgetFile "tweet/tweets_logged")
    
tweetsPageUserNotLogged :: Text -> Handler Html
tweetsPageUserNotLogged tweetuserident = do
    tweetUser <- runDB $ selectFirst [UserIdent ==. tweetuserident] []
    applicationNotLoggedLayout $ do 
        $(widgetFile "tweet/tweets_not_logged")

-- API

postCreateTweetR :: Handler Value
postCreateTweetR = do
    tweet <- requireJsonBody :: Handler Tweet
    newTweet <- runDB $ insert tweet
    sendStatusJSON created201 (object ["resp" .= (tweet)])

postTweetLikeR :: UserId -> TweetId -> Handler ()
postTweetLikeR uid tid = return ()

postTweetUnlikeR :: UserId -> TweetId -> Handler ()
postTweetUnlikeR uid tid = return ()

postTweetRetweetR :: UserId -> TweetId -> Handler ()
postTweetRetweetR uid tid = return ()

