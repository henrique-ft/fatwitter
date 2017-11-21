{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Helpers.Tweet(likesByTweetId,retweetsByTweetId) where

import Import

likesByTweetId :: TweetId -> Handler [Entity TweetLike]
likesByTweetId tweetid = runDB $ selectList [TweetLikeTweetId ==. tweetid] []

retweetsByTweetId :: TweetId -> Handler [Entity Tweet]
retweetsByTweetId tweetid = runDB $ selectList [TweetParenttweetid ==. (Just tweetid)] []