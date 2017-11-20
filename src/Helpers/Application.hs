{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Helpers.Application (applicationLayout,applicationNotLoggedLayout,redirectOut) where

import Import
import Text.Read

-- Base do layout (frameworks css/js)
baseApplicationLayout :: Widget 
baseApplicationLayout = do
    addScriptRemote "https://code.jquery.com/jquery-3.2.1.min.js"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.0.0-beta/js/bootstrap.min.js"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.19.2/moment.min.js"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.19.2/locale/pt-br.js"
    addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.0.0-beta/css/bootstrap.min.css"
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"


-- Layout principal da aplicação
applicationLayout :: Widget -> Handler Html
applicationLayout x = do
    defaultLayout $ do
        baseApplicationLayout
        $(widgetFile "layouts/application")
        x
        $(whamletFile "templates/layouts/footer.hamlet")

-- Layout não logado
applicationNotLoggedLayout :: Widget -> Handler Html 
applicationNotLoggedLayout x = do 
    userid <- lookupSession "UserId"
    case userid of
        Nothing -> do
            defaultLayout $ do
                baseApplicationLayout 
                $(widgetFile "layouts/application_not_logged")
                x
                $(whamletFile "templates/layouts/footer.hamlet")
        _ -> redirect HomeUserR

-- Redireciona para o login
redirectOut :: Handler Html
redirectOut = do
            setUltDestCurrent
            setMessage "Please, enter with your email and password."
            redirect UserLoginR