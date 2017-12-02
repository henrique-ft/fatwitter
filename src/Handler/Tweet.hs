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
import Helpers.Tweet (retweetsByTweetId, likesByTweetId, getTweetUserIdWithRetweetFilter)

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
                    tweetsnumber <- runDB $ count [TweetUserId ==. tweetuserid, TweetParenttweetid ==. Nothing]
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
            tweetsnumber <- runDB $ count [TweetUserId ==. tweetuserid, TweetParenttweetid ==. Nothing]
            applicationNotLoggedLayout $ do
                $(widgetFile "tweet/tweets_not_logged")

-- API

postCreateTweetR :: Handler Value
postCreateTweetR = do
    tweet <- requireJsonBody :: Handler Tweet
    tweetid <- runDB $ insert tweet
    sendStatusJSON created201 (object ["resp" .= (tweet),"id" .= tweetid])

getTweetsHomeR :: UserId -> Handler Value
getTweetsHomeR loggeduserid = do
    -- Users
    loggeduser <- runDB $ get404 loggeduserid :: Handler User
    -- Tweets
    tweets <- runDB $ (rawSql ("SELECT DISTINCT ?? FROM tweet INNER JOIN user_follower ON user_follower.user_id = tweet.user_id WHERE user_follower.follower_user = "<> (pack $ show $ fromSqlKey loggeduserid) <>" OR tweet.user_id = " <> (pack $ show $ fromSqlKey loggeduserid) <> "ORDER BY tweet.id ASC") [])::Handler [Entity Tweet]
    tweetsids <- return $ Prelude.map entityKey tweets
    tweetsentities <- return $ Prelude.map entityVal tweets
    -- Likes
    tweetslikes <- sequence $ Prelude.map likesByTweetId tweetsids 
    tweetsloggeduserlikeentity <- runDB $ selectList [TweetLikeUserId ==. loggeduserid] []
    tweetsloggeduserlikeval <- return $ Prelude.map entityVal tweetsloggeduserlikeentity
    tweetsloggeduserlike <- return $ Prelude.map tweetLikeTweetId tweetsloggeduserlikeval
    -- Retweets
    tweetsretweets <- sequence $ Prelude.map retweetsByTweetId tweetsids
    tweetsloggeduserretweetentity <- runDB $ selectList [TweetUserId ==. loggeduserid, TweetIsretweet ==. True] []
    tweetsloggeduserretweetval <- return $ Prelude.map entityVal tweetsloggeduserretweetentity
    tweetsloggeduserretweet <- return $ Prelude.map tweetParenttweetid tweetsloggeduserretweetval
    -- Users
    tweetsusersids <- sequence $ Prelude.map getTweetUserIdWithRetweetFilter tweetsentities
    tweetsusers <- sequence $ Prelude.map (\userid -> runDB $ selectFirst [UserId ==. userid] []) tweetsusersids    
    sendStatusJSON ok200 (object ["tweets" .= tweets, "tweetlikes" .= tweetslikes, "tweetsretweets" .= tweetsretweets, "tweetsusers" .= tweetsusers, "tweetsloggeduserlike" .= tweetsloggeduserlike, "tweetsloggeduserretweet" .= tweetsloggeduserretweet, "loggeduser" .= loggeduser, "loggeduserid" .= loggeduserid])

getTweetsUserLoggedR :: UserId -> UserId -> Handler Value
getTweetsUserLoggedR loggeduserid tweetsuserid = do
    -- Users
    user <- runDB $ get404 tweetsuserid 
    loggeduser <- runDB $ get404 loggeduserid :: Handler User
    -- Tweets
    tweets <- runDB $ selectList [TweetUserId ==. tweetsuserid] []
    tweetsids <- return $ Prelude.map entityKey tweets
    tweetsentities <- return $ Prelude.map entityVal tweets
    -- Likes
    tweetslikes <- sequence $ Prelude.map likesByTweetId tweetsids 
    tweetsloggeduserlikeentity <- runDB $ selectList [TweetLikeUserId ==. loggeduserid] []
    tweetsloggeduserlikeval <- return $ Prelude.map entityVal tweetsloggeduserlikeentity
    tweetsloggeduserlike <- return $ Prelude.map tweetLikeTweetId tweetsloggeduserlikeval
    -- Retweets
    tweetsretweets <- sequence $ Prelude.map retweetsByTweetId tweetsids
    tweetsloggeduserretweetentity <- runDB $ selectList [TweetUserId ==. loggeduserid, TweetIsretweet ==. True] []
    tweetsloggeduserretweetval <- return $ Prelude.map entityVal tweetsloggeduserretweetentity
    tweetsloggeduserretweet <- return $ Prelude.map tweetParenttweetid tweetsloggeduserretweetval
    -- Users
    tweetsusersids <- sequence $ Prelude.map getTweetUserIdWithRetweetFilter tweetsentities
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
    tweetsusersids <- sequence $ Prelude.map getTweetUserIdWithRetweetFilter tweetsentities
    -- Users
    tweetsusers <- sequence $ Prelude.map (\uid -> runDB $ selectFirst [UserId ==. uid] []) tweetsusersids
    sendStatusJSON ok200 (object ["resp" .= tweets, "user" .= user, "tweetsusers" .= tweetsusers])

postTweetLikeR :: Handler Value
postTweetLikeR = do 
    tweetlike <- requireJsonBody :: Handler TweetLike
    newTweet <- runDB $ insert tweetlike
    sendStatusJSON created201 (object ["resp" .= (tweetlike)])

deleteTweetUnlikeR :: Handler Value
deleteTweetUnlikeR = do
    tweetunlike <- requireJsonBody :: Handler TweetLike
    runDB $ deleteWhere [TweetLikeUserId ==. (tweetLikeUserId tweetunlike), TweetLikeTweetId ==. (tweetLikeTweetId tweetunlike)]
    sendStatusJSON noContent204 (object ["tweetunlike" .= tweetunlike])

postTweetRetweetR :: Handler Value
postTweetRetweetR = do
    tweet <- requireJsonBody :: Handler Tweet
    newTweet <- runDB $ insert tweet
    sendStatusJSON created201 (object ["resp" .= (tweet)])
    
deleteTweetUnretweetR :: Handler Value
deleteTweetUnretweetR = do
    untweet <- requireJsonBody :: Handler Tweet
    runDB $ deleteWhere [TweetUserId ==. (tweetUserId untweet), TweetParenttweetid ==. (tweetParenttweetid untweet)]
    sendStatusJSON noContent204 (object ["tweetunretweet" .= untweet])

deleteTweetDeleteR :: Handler Value
deleteTweetDeleteR = do
    deletetweetjson <- requireJsonBody :: Handler Tweet
    deletetweet <- runDB $ selectFirst [TweetUserId ==. (tweetUserId deletetweetjson), TweetDescription ==. (tweetDescription deletetweetjson)] []
    case deletetweet of
        Nothing -> notFound 
        Just (Entity tweetid _) -> do
            runDB $ deleteWhere [TweetLikeTweetId ==. tweetid]
            runDB $ deleteWhere [TweetParenttweetid ==. Just tweetid]
            runDB $ delete tweetid 
            sendStatusJSON noContent204 (object ["tweetdeleted" .= deletetweetjson])
