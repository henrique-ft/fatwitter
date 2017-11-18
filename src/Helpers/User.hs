{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Helpers.User(formUser, formAuthUser, AuthUser, emailAuthUser, passwordAuthUser)where

import qualified Data.Text as DT

import Import

data AuthUser = AuthUser {emailAuthUser :: Text, passwordAuthUser :: Text} deriving Show

formUser :: Form User 
formUser = renderBootstrap $ User 
        <$> areq textField "Nickname: " Nothing
        <*> areq passwordField "Password: " Nothing
        <*> aopt textField "Description: " Nothing
        <*> areq emailField  "Email: " Nothing

formAuthUser :: Form AuthUser 
formAuthUser = renderBootstrap $ AuthUser 
        <$> areq emailField  "Email: " Nothing
        <*> areq passwordField "Password: " Nothing
