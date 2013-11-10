module Handler.Join where

import Import

getJoinR :: Handler Html
getJoinR = do
    (widget, enctype) <- generateFormPost joinForm
    defaultLayout $ do
        setTitle "join shoesha.re"
        $(widgetFile "join")

postJoinR :: Handler Html
postJoinR = error "postJoinR: not implemented"

joinForm :: Form User
joinForm ex = do
    (emailResult, emailView) <- mreq emailField "butts" Nothing
    (passResult, passView) <- mreq passwordField "butts" Nothing
    return undefined
