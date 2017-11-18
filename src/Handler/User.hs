{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.User where

import Import
import Control.Applicative
import qualified Data.Text as DT
import Database.Persist.Postgresql

-- HELPERS

import Helpers.Application (applicationLayout,applicationNotLoggedLayout)
import Helpers.User (formUser)

-- WEB

getHomeUserR :: Handler Html
getHomeUserR = applicationLayout $ do 
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

-- API

postFollowUserR :: UserId -> UserId -> Handler ()
postFollowUserR uidFollowing uidFollowed = return ()

postUnfollowUserR :: UserId -> UserId -> Handler ()
postUnfollowUserR uidFollowing uidFollowed = return ()
