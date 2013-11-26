{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Handler.Join where

import Control.Arrow
import Control.Monad
import Crypto.PasswordStore
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as B (fromStrict)
import Data.Char
import Data.ISO3166_CountryCodes
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.String
import Data.Digest.Pure.SHA
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
        FormSuccess person -> do
            pw <- liftIO $ makePassword (userPassword person) 14
            let newPerson = person { userPassword = pw }
            _ <- runDB (insert newPerson)
            setSession "email" (userEmail newPerson)
            redirect WelcomeR
        _ -> defaultLayout $ do
           setTitle "try again"
           $(widgetFile "join")

joinForm :: Form User
joinForm ex = do
    (emailResult, emailView) <-
        mreq emailField
             (formControlAutofocus "Email")
             Nothing
    (passResult, passView) <-
        mreq (checkBool ((>= 8) . B.length) ("Too short! (must be 8 or more characters)" :: Text)
               passwordConfirmField)
             (formControl "Password")
             Nothing
    (usernameResult, usernameView) <-
        mreq (checkBool (not . T.any isSpace) ("Spaces not allowed!" :: Text)
               textField)
             (formControl "Username (no spaces)")
             Nothing
    (puddingResult, puddingView) <-
        mreq (selectField defaultOptionsEnum)
             selectOpts
             Nothing
    (countryResult, countryView) <-
        mreq (selectField countryOptions)
             selectOpts
             Nothing
    verkey <- liftIO $ withBinaryFile "/dev/urandom" ReadMode $ \h ->
        B.pack . show . sha256 . B.fromStrict <$> B.hGet h 18

    let user = User <$> emailResult
                    <*> usernameResult
                    <*> passResult
                    <*> puddingResult
                    <*> countryResult
                    <*> pure verkey
    return (user, $(widgetFile "join-form"))

    where
        selectOpts = "" { fsAttrs =
                            [ ("data-style", "btn-default")
                            , ("data-live-search", "true")
                            ] }
        puddings = map (pack . show &&& id) [minBound..maxBound :: PuddingType]
        formControlAutofocus (formControl -> f) =
            f { fsAttrs = fsAttrs f ++ [("autofocus", "true")] }
        formControl pl = ""
                   { fsLabel = fromString pl
                   , fsAttrs = [("class", "form-control"), ("placeholder", fromString pl)]
                   }
        defaultOptionsEnum = fmap (prepend (Option "Favorite pudding" VanillaPudding "no pudding")) optionsEnum
        prepend m (OptionList a b) = OptionList (m:a) b
        countryOptions = fmap (prepend nullOption)
                       $ optionsPairs $ map (pack . readableCountryName &&& id) [minBound..maxBound]
        nullOption = Option "Country of residence" AF "no country"

passwordConfirmField :: Monad m => Field m B.ByteString
passwordConfirmField = Field
    { fieldParse = \vals _ -> case vals of
        [x, y] | x == y -> return . Right . Just $ T.encodeUtf8 x
               | otherwise -> return $ Left "Passwords don't match"
        [] -> return $ Right Nothing
        _ -> return $ Left "Incorrect number of results"
    , fieldView = \id' name attrs val' isReq ->
        let val = case val' of Right e -> T.decodeUtf8 e; Left _ -> ""
         in [whamlet|
        <label for=#{id'} .sr-only>Password
        <input ##{id'} name=#{name} value=#{val} *{attrs} type=password :isReq:required>
        <label for=#{id'}-confirm .sr-only>Password (again)
        <input ##{id'}-confirm name=#{name} value=#{val} *{attrs} placeholder="Password (again)" type=password :isReq:required>
      |]
    , fieldEnctype = UrlEncoded
    }
