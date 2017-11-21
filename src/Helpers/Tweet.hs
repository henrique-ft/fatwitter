{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Helpers.Tweet(likesByTweetId,retweetsByTweetId,getTweetUserIdWithRetweetFilter) where

import Import

likesByTweetId :: TweetId -> Handler [Entity TweetLike]
likesByTweetId tweetid = do 
    tweet <- runDB $ get404 tweetid
    case (tweetParenttweetid tweet) of
        Nothing -> runDB $ selectList [TweetLikeTweetId ==. tweetid] []
        Just parenttweetid -> runDB $ selectList [TweetLikeTweetId ==. parenttweetid] []

retweetsByTweetId :: TweetId -> Handler [Entity Tweet]
retweetsByTweetId tweetid = do 
    tweet <- runDB $ get404 tweetid
    case (tweetParenttweetid tweet) of
        Nothing -> runDB $ selectList [TweetParenttweetid ==. (Just tweetid)] []
        Just parenttweetid -> runDB $ selectList [TweetParenttweetid ==. (Just parenttweetid)] []

getTweetUserIdWithRetweetFilter :: Tweet -> Handler UserId
getTweetUserIdWithRetweetFilter tweet = do 
    case (tweetParenttweetid tweet) of
        Nothing -> return (tweetUserId tweet)
        Just parenttweetid -> do
            parenttweet <- runDB $ get404 parenttweetid
            return (tweetUserId parenttweet) 