{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Helpers.User(formUser)where

import Import

formUser :: Form User 
formUser = renderBootstrap $ User 
        <$> areq textField "Nickname: " Nothing
        <*> areq passwordField "Password: " Nothing
        <*> aopt textField "Description: " Nothing
        <*> areq emailField  "Email: " Nothing
