{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Tweet where

import Import
import Data.Text

-- HELPERS

import Helpers.Application (applicationLayout)

-- WEB

getTweetsR :: Text -> Handler Html
getTweetsR uident = applicationLayout $ do 
    $(widgetFile "tweet/tweets")

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

