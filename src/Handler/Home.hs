{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import Import
-- import Network.HTTP.Types.Status
-- import Database.Persist.Postgresql

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

-- API

postAuthenticationR :: Handler ()
postAuthenticationR = return ()