{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Helpers.User(formUser, 
                    formAuthUser,
                    AuthUser, 
                    EditUser,
                    editUserName,
                    editUserIdent,
                    editUserColor,
                    editUserDescription,
                    editUserEmail,
                    emailAuthUser, 
                    passwordAuthUser,
                    isLoggedUserFollowing, 
                    isLoggedUserSameThan,
                    formEditUser)where

import qualified Data.Text as DT

import Import

data AuthUser = AuthUser {emailAuthUser :: Text, passwordAuthUser :: Text} deriving Show
data EditUser = EditUser {editUserName :: Text, editUserIdent :: Text, editUserColor :: Text, editUserDescription :: (Maybe Text), editUserEmail :: Text}

formUser :: Form User 
formUser = renderBootstrap $ User 
        <$> areq textField "Name:" Nothing
        <*> areq textField "Nickname (@): " Nothing
        <*> areq passwordField "Password: " Nothing
        <*> areq textField "Your Color: " Nothing
        <*> aopt textField "Description: " Nothing
        <*> areq emailField  "Email: " Nothing

formEditUser :: User -> Form EditUser 
formEditUser user = renderBootstrap $ EditUser 
        <$> areq textField "Name:" (Just (userName user))
        <*> areq textField "Nickname (@): " (Just (userIdent user))
        <*> areq textField "Your Color: " (Just (userColor user))
        <*> aopt textField "Description: " (Just (userDescription user))
        <*> areq emailField  "Email: " (Just (userEmail user))

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