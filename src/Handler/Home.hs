{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import Import
-- import Network.HTTP.Types.Status
-- import Database.Persist.Postgresql

-- HELPERS

import Helpers.Application (applicationLayout)

-- WEB

getHomeR :: Handler Html 
getHomeR = applicationLayout $ do
    $(widgetFile "home")