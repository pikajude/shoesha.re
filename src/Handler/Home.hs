module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "find treats for your feet."
    $(widgetFile "homepage")
