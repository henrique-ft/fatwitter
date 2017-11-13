{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import Import
import qualified Data.Text as DT
import Database.Persist.Postgresql

-- HELPERS

import Helpers.Application (applicationNotLoggedLayout)
import Helpers.Home

-- WEB

getHomeR :: Handler Html 
getHomeR = applicationNotLoggedLayout $ do
    $(widgetFile "home")
    
getUserLoginR :: Handler Html
getUserLoginR = applicationNotLoggedLayout $ do
    $(widgetFile "login")

postAuthenticationR :: Handler Value
postAuthenticationR = do
    user <- requireJsonBody :: Handler User
    auth <- runDB $ selectFirst [UserEmail ==. (userEmail user), UserPassword ==. (userPassword user)] []
    case auth of
        nothing -> do
            redirect UserLoginR
        Just (Entity userId user) -> do
            setSession "UserId" (DT.pack (show (fromSqlKey userId)))
            redirect HomeR
