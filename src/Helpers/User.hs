{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
                    formEditUser,
                    validateIdentAlreadyExists,
                    validateEmailAlreadyExists,
                    formUploadProfileImage)where

import qualified Data.Text as DT

import Import

data AuthUser = AuthUser {emailAuthUser :: Text, passwordAuthUser :: Text} deriving Show
data EditUser = EditUser {editUserName :: Text, editUserIdent :: Text, editUserColor :: Text, editUserDescription :: (Maybe Text), editUserEmail :: Text}

-- Forms

formUploadProfileImage :: Form FileInfo
formUploadProfileImage = renderDivs $ areq fileField 
                           FieldSettings{fsId=Just "hident1",
                                         fsLabel="Arquivo: ",
                                         fsTooltip= Nothing,
                                         fsName= Nothing,
                                         fsAttrs=[("accept","image/jpeg")]} 
                           Nothing


formUser :: Form User 
formUser = renderBootstrap $ User 
        <$> areq textField "Name:" Nothing
        <*> areq textField "Nickname (@): " Nothing
        <*> areq passwordField "Password: " Nothing
        <*> areq textField "Your Color: " Nothing
        <*> aopt textField "Description: " Nothing
        <*> areq emailField  "Email: " Nothing
        <*> aopt hiddenField "" Nothing

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

-- Others

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


-- Validation for ident
validateIdentAlreadyExists :: User -> Maybe EditUser -> Handler Bool
validateIdentAlreadyExists user Nothing = do
    result <- runDB $ selectFirst [UserIdent ==. (userIdent user)] []
    case result of
        Nothing -> return True
        Just user -> do
            setMessage "This nickname are already in use !"
            return False
validateIdentAlreadyExists loggeduser (Just edituser) = do
    result <- runDB $ selectFirst [UserIdent ==. (editUserIdent edituser)] []
    case result of
        Nothing -> return True
        Just (Entity _ user) -> do
            case ((userIdent user) == (userIdent loggeduser)) of
                True -> return True
                False -> do
                    setMessage "This nickname are already in use !"
                    return False

-- Validation for email
validateEmailAlreadyExists :: User -> Maybe EditUser -> Handler Bool
validateEmailAlreadyExists user Nothing = do
    result <- runDB $ selectFirst [UserEmail ==. (userEmail user)] []
    case result of
        Nothing -> return True
        Just user -> do
            setMessage "This email are already in use !"
            return False
validateEmailAlreadyExists loggeduser (Just edituser) = do
    result <- runDB $ selectFirst [UserEmail ==. (editUserEmail edituser)] []
    case result of
        Nothing -> return True
        Just (Entity _ user) -> do
            case ((userEmail user) == (userEmail loggeduser)) of
                True -> return True
                False ->do
                    setMessage "This email are already in use !"
                    return False