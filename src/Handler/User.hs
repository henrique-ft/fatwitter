{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.User where

import Import

-- WEB

getHomeUserR :: Handler Html
getHomeUserR = defaultLayout $ do 
    $(widgetFile "user/home")

getNewUserR :: Handler Html
getNewUserR = defaultLayout $ do 
    $(widgetFile "user/new")

postCreateUserR :: Handler Html
postCreateUserR = defaultLayout $ do 
    $(widgetFile "user/new")

getEditUserR :: Handler Html
getEditUserR = defaultLayout $ do 
    $(widgetFile "user/edit")

postUpdateUserR :: UserId -> Handler Html
postUpdateUserR uid = defaultLayout $ do 
    $(widgetFile "user/edit")

-- API

postFollowUserR :: UserId -> UserId -> Handler ()
postFollowUserR uidFollowing uidFollowed = return ()

postUnfollowUserR :: UserId -> UserId -> Handler ()
postUnfollowUserR uidFollowing uidFollowed = return ()
