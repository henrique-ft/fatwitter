{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes           #-}
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
                    followingnumber <- runDB $ count [UserFollowerFollowerUser ==. tweetuserid]
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
            followingnumber <- runDB $ count [UserFollowerFollowerUser ==. tweetuserid]
            applicationNotLoggedLayout $ do
                $(widgetFile "tweet/tweets_not_logged")

-- API

postCreateTweetR :: Handler Value
postCreateTweetR = do
    tweet <- requireJsonBody :: Handler Tweet
    newTweet <- runDB $ insert tweet
    sendStatusJSON created201 (object ["resp" .= (tweet)])

getTweetsUserLoggedR :: UserId -> UserId -> Handler Value
getTweetsUserLoggedR tweetsuserid loggeduserid = do
    -- Users
    user <- runDB $ get404 tweetsuserid 
    loggeduser <- runDB $ get404 loggeduserid :: Handler User
    -- Tweets
    tweets <- runDB $ selectList [TweetUserId ==. tweetsuserid] []
    tweetsids <- return $ Prelude.map entityKey tweets
    -- Likes
    tweetslikes <- sequence $ Prelude.map (\tweetid -> runDB $ selectList [TweetLikeTweetId ==. tweetid] []) tweetsids
    tweetsloggeduserlikeentity <- runDB $ selectList [TweetLikeUserId ==. tweetsuserid] []
    tweetsloggeduserlike <- return $ Prelude.map entityKey tweetsloggeduserlikeentity
    -- Retweets
    tweetsretweets <- sequence $ Prelude.map (\tweetid -> runDB $ selectList [TweetParenttweetid ==. (Just tweetid)] []) tweetsids
    tweetsloggeduserretweetentity <- runDB $ selectList [TweetUserId ==. tweetsuserid, TweetIsretweet ==. True] []
    tweetsloggeduserretweet <- return $ Prelude.map entityKey tweetsloggeduserretweetentity
    -- Response
    sendStatusJSON ok200 (object ["tweets" .= tweets, "tweetlikes" .= tweetslikes, "tweetsretweets" .= tweetsretweets, "tweetsloggeduserlike" .= tweetsloggeduserlike, "tweetsloggeduserretweet" .= tweetsloggeduserretweet, "loggeduser" .= loggeduser, "loggeduserid" .= loggeduserid, "tweetuser" .= user, "tweetuserid" .= tweetsuserid])

getTweetsUserUnloggedR :: UserId -> Handler Value
getTweetsUserUnloggedR userid = do
    user <- runDB $ get404 userid 
    tweets <- runDB $ selectList [TweetUserId ==. userid] []
    sendStatusJSON ok200 (object ["resp" .= tweets, "user" .= user])

getTweetsHomeR :: UserId -> Handler ()
getTweetsHomeR userid = return ()

postTweetLikeR :: UserId -> TweetId -> Handler ()
postTweetLikeR uid tid = return ()

postTweetUnlikeR :: UserId -> TweetId -> Handler ()
postTweetUnlikeR uid tid = return ()

postTweetRetweetR :: UserId -> TweetId -> Handler ()
postTweetRetweetR uid tid = return ()

