{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Tweet where

import Import
import Data.Maybe
import qualified Data.Text as DT
import Database.Persist.Postgresql
import Control.Applicative

-- HELPERS

import Helpers.Application (applicationLayout, applicationNotLoggedLayout, redirectOut)
import Helpers.User (isLoggedUserSameThan, isLoggedUserFollowing)

-- WEB

getTweetsPageR :: Text -> Handler Html
getTweetsPageR tweetuserident = do
    userid <- lookupSession "UserId"
    case userid of
        Nothing -> (tweetsPageUserNotLogged tweetuserident)
        Just userid -> (tweetsPageUserLogged tweetuserident)

tweetsPageUserLogged :: Text -> Handler Html
tweetsPageUserLogged tweetuserident = do
    userid <- lookupSession "UserId"
    case userid of
        Nothing -> redirectOut
        Just userid -> do
            loggeduser <- runDB $ get404 (read (unpack (userid))) :: Handler User
            tweetuser <- runDB $ selectFirst [UserIdent ==. tweetuserident] []
            case tweetuser of
                Nothing -> notFound
                Just (Entity tweetuserid tweetuser) -> do
                    isloggeduserfollowing <- isLoggedUserFollowing tweetuserid
                    isloggedusersamethan <- isLoggedUserSameThan tweetuserid
                    followersnumber <- runDB $ count [UserFollowerUserId ==. tweetuserid]
                    loggeduserid <- return (read (unpack userid)) :: Handler UserId
                    applicationLayout $ do 
                        $(widgetFile "tweet/tweets_logged")

tweetsPageUserNotLogged :: Text -> Handler Html
tweetsPageUserNotLogged tweetuserident = do
    tweetuser <- runDB $ selectFirst [UserIdent ==. tweetuserident] []
    case tweetuser of
        Nothing -> notFound
        Just (Entity tweetuserid tweetuser) -> do
            followersnumber <- runDB $ count [UserFollowerUserId ==. tweetuserid]
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

