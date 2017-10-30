{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Tweet where

import Import

import Data.Text

-- WEB

getTweetsR :: Text -> Handler Html
getTweetsR uident = defaultLayout $ do 
    $(widgetFile "tweet/tweets")

-- API

postTweetLikeR :: UserId -> TweetId -> Handler ()
postTweetLikeR uid tid = return ()

postTweetUnlikeR :: UserId -> TweetId -> Handler ()
postTweetUnlikeR uid tid = return ()

postTweetRetweetR :: UserId -> TweetId -> Handler ()
postTweetRetweetR uid tid = return ()

