{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Handler.Join where

import Control.Arrow
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Text (pack)
import Data.Text.Encoding
import Data.Digest.Pure.SHA
import qualified Data.Text as T
import Data.String
import Model.Types
import System.IO
import Text.Julius
import Import

getJoinR :: Handler Html
getJoinR = do
    (widget, enctype) <- generateFormPost joinForm
    defaultLayout $ do
        setTitle "join shoesha.re"
        $(widgetFile "join")

postJoinR :: Handler Html
postJoinR = do
    ((res, widget), enctype) <- runFormPost joinForm
    case res of
        FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
        _ -> defaultLayout $ do
           setTitle "try again"
           $(widgetFile "join")

joinForm :: Form User
joinForm ex = do
    (emailResult, emailView) <- mreq emailField (formControlAutofocus "Email") Nothing
    (passResult, passView) <- mreq passwordField (formControl "Password") Nothing
    (passConfirmResult, passConfirmView) <- mreq (checkBool ((>= 8) . T.length) ("Too short! (<8 characters)" :: Text) passwordField) (formControl "Password (again)") Nothing
    (usernameResult, usernameView) <- mreq (checkBool (not . T.any isSpace) ("Spaces not allowed!" :: Text) textField) (formControl "Username (no spaces)") Nothing
    let selectOpts = "" { fsAttrs =
                            [ ("data-style", "btn-success")
                            , ("data-live-search", "true")
                            ] }
    (puddingResult, puddingView) <- mreq (selectField defaultOptionsEnum) selectOpts Nothing
    bytes <- liftIO $ withBinaryFile "/dev/urandom" ReadMode (fmap (decodeUtf8 . B.toStrict . bytestringDigest . sha1) . flip B.hGet 36)
    let user = User
            <$> emailResult
            <*> usernameResult
            <*> passResult
            <*> pure bytes
            <*> puddingResult
    return (user, $(widgetFile "join-form"))
    where
        puddings = map (pack . show &&& id) [minBound..maxBound :: PuddingType]
        formControlAutofocus (formControl -> f) =
            f { fsAttrs = fsAttrs f ++ [("autofocus", "true")] }
        formControl pl = ""
                   { fsLabel = fromString pl
                   , fsAttrs = [("class", "form-control"), ("placeholder", fromString pl)]
                   }
        defaultOptionsEnum = fmap (prepend (Option "Favorite pudding" VanillaPudding "no pudding")) optionsEnum
            where prepend m (OptionList a b) = OptionList (m:a) b
