{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Helpers.User(formUser, 
                    formAuthUser,
                    AuthUser, 
                    emailAuthUser, 
                    passwordAuthUser,
                    isLoggedUserFollowing, 
                    isLoggedUserSameThan)where

import qualified Data.Text as DT

import Import

data AuthUser = AuthUser {emailAuthUser :: Text, passwordAuthUser :: Text} deriving Show

formUser :: Form User 
formUser = renderBootstrap $ User 
        <$> areq textField "Name:" Nothing
        <*> areq textField "Nickname (@): " Nothing
        <*> areq passwordField "Password: " Nothing
        <*> areq textField "Your Color: " Nothing
        <*> aopt textField "Description: " Nothing
        <*> areq emailField  "Email: " Nothing

formAuthUser :: Form AuthUser 
formAuthUser = renderBootstrap $ AuthUser 
        <$> areq emailField  "Email: " Nothing
        <*> areq passwordField "Password: " Nothing

isLoggedUserFollowing :: UserId -> Handler Bool
isLoggedUserFollowing userid = do
    currentuserid <- lookupSession "UserId"
    case currentuserid of
        Nothing -> return False
        Just currentuserid -> do
            tweetuser <- runDB $ selectFirst [UserFollowerUserId ==. userid, UserFollowerFollowerUser ==. (read (unpack (currentuserid)))] []
            case tweetuser of
                Nothing -> return False
                Just _ -> return True

isLoggedUserSameThan :: UserId -> Handler Bool
isLoggedUserSameThan userid = do
    currentuserid <- lookupSession "UserId"
    case currentuserid of
        Nothing -> return False
        Just currentuserid -> return (userid == (read (unpack (currentuserid))))

--(read (unpack (lookupSession "UserId")))