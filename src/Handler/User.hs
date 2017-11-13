{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.User where

import Import
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

-- HELPERS

import Helpers.Application (applicationLayout,applicationNotLoggedLayout)

-- WEB

getHomeUserR :: Handler Html
getHomeUserR = applicationLayout $ do 
    $(widgetFile "user/home")

getNewUserR :: Handler Html
getNewUserR = applicationNotLoggedLayout $ do 
    $(widgetFile "user/new")

postCreateUserR :: Handler Value
postCreateUserR = do
    user <- requireJsonBody :: Handler User
    newUser <- runDB $ insert user
    sendStatusJSON created201 (object ["resp" .= (user)])

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
