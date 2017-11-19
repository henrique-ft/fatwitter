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

-- API

postFollowUserR :: UserId -> UserId -> Handler ()
postFollowUserR uidFollowing uidFollowed = return ()

postUnfollowUserR :: UserId -> UserId -> Handler ()
postUnfollowUserR uidFollowing uidFollowed = return ()
