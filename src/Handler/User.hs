{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.User where

import Import
import Data.Maybe
import Control.Applicative
import qualified Data.Text as DT
import Database.Persist.Postgresql

-- HELPERS

import Helpers.Application (applicationLayout,applicationNotLoggedLayout,redirectOut)
import Helpers.User (formUser)

-- WEB

getHomeUserR :: Handler Html
getHomeUserR = do 
    userid <- lookupSession "UserId"
    case userid of
        Nothing -> redirectOut
        Just userid -> do
            loggeduser <- runDB $ get404 (read (unpack (userid)))
            followersnumber <- runDB $ count [UserFollowerUserId ==. (read (unpack (userid)))]
            followingnumber <- runDB $ count [UserFollowerFollowerUser ==. (read (unpack (userid)))]
            loggeduserid <- return (read (unpack userid)) :: Handler UserId
            applicationLayout $ do 
                $(widgetFile "user/home")

getNewUserR :: Handler Html
getNewUserR = do
    (widget,enctype) <- generateFormPost formUser
    applicationNotLoggedLayout $ do 
        $(widgetFile "user/new")

postCreateUserR :: Handler Value
postCreateUserR = do
    ((result,_),_) <- runFormPost formUser
    case result of 
        FormSuccess user -> do 
            userid <- runDB $ insert user 
            setSession "UserId" (DT.pack (show userid))
            redirect HomeUserR
        _ -> redirect NewUserR

getEditUserR :: Handler Html
getEditUserR = applicationLayout $ do 
    $(widgetFile "user/edit")

postUpdateUserR :: UserId -> Handler Html
postUpdateUserR uid = applicationLayout $ do 
    $(widgetFile "user/edit")

getFollowersUserR :: Handler Html
getFollowersUserR = do 
    userid <- lookupSession "UserId"
    case userid of
        Nothing -> redirectOut
        Just userid -> do
            loggeduser <- runDB $ get404 (read (unpack (userid)))
            followersnumber <- runDB $ count [UserFollowerUserId ==. (read (unpack (userid)))]
            followingnumber <- runDB $ count [UserFollowerFollowerUser ==. (read (unpack (userid)))]
            loggeduserid <- return (read (unpack userid)) :: Handler UserId
            followersuserentity <- runDB $ selectList [UserFollowerUserId ==. (read (unpack (userid)))] []
            followersusersids <- return $ Prelude.map (\x -> userFollowerFollowerUser (entityVal x)) followersuserentity
            followersusers <- sequence $ Prelude.map (\followeruserid -> runDB $ get404 followeruserid) followersusersids
            applicationLayout $ do 
                $(widgetFile "user/followers")


-- API

postFollowUserR :: UserId -> UserId -> Handler Value
postFollowUserR userfollowingid userfollowedid = do 
    userfollowerid <- runDB $ insert (UserFollower userfollowedid userfollowingid)
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey userfollowerid)])


deleteUnfollowUserR :: UserId -> UserId -> Handler Value
deleteUnfollowUserR userfollowingid userfollowedid = do
    runDB $ deleteWhere [UserFollowerUserId ==. userfollowedid, UserFollowerFollowerUser ==. userfollowingid]
    sendStatusJSON noContent204 (object ["userfollowingiddeleted" .= userfollowingid, "userfollowediddeleted" .= userfollowedid])

