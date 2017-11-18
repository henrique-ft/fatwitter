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
import Helpers.User (formAuthUser, AuthUser, emailAuthUser, passwordAuthUser)
import Helpers.Home

-- WEB

getHomeR :: Handler Html 
getHomeR = applicationNotLoggedLayout $ do
    $(widgetFile "home")
    
getUserLoginR :: Handler Html
getUserLoginR = do
    (widget,enctype) <- generateFormPost formAuthUser
    applicationNotLoggedLayout $ do
        $(widgetFile "login")

postAuthenticationR :: Handler Value
postAuthenticationR = do
    ((result,_),_) <- runFormPost formAuthUser
    case result of 
        FormSuccess authUser -> do 
            auth <- runDB $ selectFirst [UserEmail ==. (emailAuthUser authUser), UserPassword ==. (passwordAuthUser authUser)] []
            case auth of
                Nothing -> do
                    redirect UserLoginR
                Just (Entity userId user) -> do
                    setSession "UserId" (DT.pack (show (fromSqlKey userId)))
                    redirect HomeUserR            
            
        _ -> redirect UserLoginR

