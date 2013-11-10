{-# LANGUAGE ViewPatterns #-}

module Handler.Join where

import Control.Arrow
import Data.Text (pack)
import Data.String
import Model.Types
import Text.Julius
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
    (emailResult, emailView) <- mreq emailField (formControlAutofocus "Email") Nothing
    (passResult, passView) <- mreq passwordField (formControl "Password") Nothing
    (passConfirmResult, passConfirmView) <- mreq passwordField (formControl "Password (again)") Nothing
    (usernameResult, usernameView) <- mreq textField (formControl "Username (no spaces)") Nothing
    let selectOpts = "" { fsAttrs =
                            [ ("data-style", "btn-success")
                            , ("data-live-search", "true")
                            ] }
    (puddingResult, puddingView) <- mreq (selectFieldList puddings) selectOpts Nothing
    let user = User
            <$> emailResult
            <*> usernameResult
            <*> passResult
            <*> pure "foo"
            <*> puddingResult
    return (user, $(widgetFile "join-form") >> toWidget $(juliusFile "templates/join-form.julius"))
    where
        puddings = map (pack . show &&& id) [minBound..maxBound :: PuddingType]
        formControlAutofocus (formControl -> f) =
            f { fsAttrs = fsAttrs f ++ [("autofocus", "true")] }
        formControl pl = ""
                   { fsLabel = fromString pl
                   , fsAttrs = [("class", "form-control"), ("placeholder", fromString pl)]
                   }
