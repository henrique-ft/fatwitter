{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.User where

import Import

-- WEB

getHomeUserR :: Handler Html
getHomeUserR = defaultLayout [whamlet|getHomeUserR|]

getNewUserR :: Handler Html
getNewUserR = defaultLayout [whamlet|getNewUserR|]

postCreateUserR :: Handler Html
postCreateUserR = defaultLayout [whamlet|postCreateUserR|]

getEditUserR :: Handler Html
getEditUserR = defaultLayout [whamlet|getEditUserR|]

postUpdateUserR :: UserId -> Handler Html
postUpdateUserR uid = defaultLayout [whamlet|postUpdateUserR|]

-- API

postFollowUserR :: UserId -> UserId -> Handler ()
postFollowUserR uidFollowing uidFollowed = return ()

postUnfollowUserR :: UserId -> UserId -> Handler ()
postUnfollowUserR uidFollowing uidFollowed = return ()
