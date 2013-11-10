module Handler.Welcome where

import Import

getWelcomeR :: Handler Html
getWelcomeR = do
    em <- lookupSession "email"
    case em of
        Just e -> do
            deleteSession "email"
            defaultLayout $(widgetFile "welcome")
        _ -> redirect HomeR
