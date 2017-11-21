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
import Helpers.Tweet (retweetsByTweetId, likesByTweetId)

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
getTweetsUserLoggedR loggeduserid tweetsuserid = do
    -- Users
    user <- runDB $ get404 tweetsuserid 
    loggeduser <- runDB $ get404 loggeduserid :: Handler User
    -- Tweets
    tweets <- runDB $ selectList [TweetUserId ==. tweetsuserid] []
    tweetsids <- return $ Prelude.map entityKey tweets
    tweetsentities <- return $ Prelude.map entityVal tweets
    tweetsusersids <- return $ Prelude.map tweetUserId tweetsentities
    -- Likes
    tweetslikes <- sequence $ Prelude.map likesByTweetId tweetsids -- Essa função deve ser mudada
    tweetsloggeduserlikeentity <- runDB $ selectList [TweetLikeUserId ==. tweetsuserid] []
    tweetsloggeduserlike <- return $ Prelude.map entityKey tweetsloggeduserlikeentity
    -- Retweets
    tweetsretweets <- sequence $ Prelude.map retweetsByTweetId tweetsids -- Essa função deve ser mudada
    tweetsloggeduserretweetentity <- runDB $ selectList [TweetUserId ==. tweetsuserid, TweetIsretweet ==. True] []
    tweetsloggeduserretweet <- return $ Prelude.map entityKey tweetsloggeduserretweetentity
    -- Users
    tweetsusers <- sequence $ Prelude.map (\userid -> runDB $ selectFirst [UserId ==. userid] []) tweetsusersids
    -- Response
    sendStatusJSON ok200 (object ["tweets" .= tweets, "tweetlikes" .= tweetslikes, "tweetsretweets" .= tweetsretweets, "tweetsusers" .= tweetsusers, "tweetsloggeduserlike" .= tweetsloggeduserlike, "tweetsloggeduserretweet" .= tweetsloggeduserretweet, "loggeduser" .= loggeduser, "loggeduserid" .= loggeduserid, "tweetuser" .= user, "tweetuserid" .= tweetsuserid])

getTweetsUserUnloggedR :: UserId -> Handler Value
getTweetsUserUnloggedR userid = do
    user <- runDB $ get404 userid 
    -- Tweets
    tweets <- runDB $ selectList [TweetUserId ==. userid] []
    tweetsids <- return $ Prelude.map entityKey tweets
    tweetsentities <- return $ Prelude.map entityVal tweets
    tweetsusersids <- return $ Prelude.map tweetUserId tweetsentities
    -- Users
    tweetsusers <- sequence $ Prelude.map (\uid -> runDB $ selectFirst [UserId ==. uid] []) tweetsusersids
    sendStatusJSON ok200 (object ["resp" .= tweets, "user" .= user, "tweetsusers" .= tweetsusers])

getTweetsHomeR :: UserId -> Handler ()
getTweetsHomeR userid = return ()

postTweetLikeR :: UserId -> TweetId -> Handler Value
postTweetLikeR loggeduserid tweetid = do 
    tweetlikeid <- runDB $ insert (TweetLike loggeduserid tweetid)
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey tweetlikeid)])

deleteTweetUnlikeR :: UserId -> TweetId -> Handler Value
deleteTweetUnlikeR loggeduserid tweetid = do
    runDB $ deleteWhere [TweetLikeUserId ==. loggeduserid, TweetLikeTweetId ==. tweetid]
    sendStatusJSON noContent204 (object ["userunlikeid" .= loggeduserid, "tweetid" .= tweetid])

postTweetRetweetR :: UserId -> TweetId -> Handler Value
postTweetRetweetR loggeduserid tweetid = do
    tweet <- requireJsonBody :: Handler Tweet
    newTweet <- runDB $ insert tweet
    sendStatusJSON created201 (object ["resp" .= (tweet)])

