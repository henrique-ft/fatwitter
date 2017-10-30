{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.User where

import Import

-- HELPERS

import Helpers.Application (applicationLayout)

-- WEB

getHomeUserR :: Handler Html
getHomeUserR = applicationLayout $ do 
    $(widgetFile "user/home")

getNewUserR :: Handler Html
getNewUserR = applicationLayout $ do 
    $(widgetFile "user/new")

postCreateUserR :: Handler Html
postCreateUserR = applicationLayout $ do 
    $(widgetFile "user/new")

getEditUserR :: Handler Html
getEditUserR = applicationLayout $ do 
    $(widgetFile "user/edit")

postUpdateUserR :: UserId -> Handler Html
postUpdateUserR uid = applicationLayout $ do 
    $(widgetFile "user/edit")

-- API

postFollowUserR :: UserId -> UserId -> Handler ()
postFollowUserR uidFollowing uidFollowed = return ()

postUnfollowUserR :: UserId -> UserId -> Handler ()
postUnfollowUserR uidFollowing uidFollowed = return ()
