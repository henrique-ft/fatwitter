{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Helpers.Application (applicationLayout) where

import Import

-- Layout principal da aplicação
applicationLayout :: Yesod site => WidgetT site IO () -> HandlerT site IO Html
applicationLayout x = defaultLayout $ do
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.0.0-beta/js/bootstrap.min.js"
    addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.0.0-beta/css/bootstrap.min.css"
    $(widgetFile "layouts/application")
    x
