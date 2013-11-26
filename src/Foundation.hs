module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Control.Monad (when)
import Crypto.PasswordStore
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import System.Log.FastLogger (Logger)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        lang <- lookupGetParam "lang"
        when (isJust lang) . setLanguage $ fromJust lang
        master <- getYesod
        mmsg <- getMessage
        extra <- getExtra

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/2.0.2/jquery.min.js"
            addScript $ StaticR js_bootstrap_js
            addScript $ StaticR js_bootstrap_select_js
            addScript $ StaticR js_application_js
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                , css_bootstrap_select_css ])
                -- , css_bootstrap_theme_css ])
            $(widgetFile "default-layout")

        curRoute <- getCurrentRoute
        let bodyClass = case curRoute of
                Just HomeR -> "home" :: Text
                _ -> "unknown"

        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = do
        muid <- maybeAuthId
        case muid of
            Just uid -> return $ Just uid
            Nothing -> do
                x <- case Just . UniqueEmail $ credsIdent creds of
                         Nothing -> return Nothing
                         Just u -> runDB (getBy u)
                case x of
                    Just (Entity uid _) -> return $ Just uid
                    Nothing -> loginErrorMessage (AuthR LoginR) "User not found"

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [AuthPlugin "hash" dispatch (\tm -> $(widgetFile "login-form"))] where
        dispatch "POST" ["login"] = postLoginR >>= sendResponse
        dispatch _ _ = notFound
        login = PluginR "hash" ["login"]
        postLoginR = do
            (mu,mp) <- lift . runInputPost $ (,)
                <$> iopt textField "email"
                <*> iopt textField "password"

            isValid <- lift $ fromMaybe (return False)
                        (validateUser <$> (Just . UniqueEmail =<< mu) <*> (T.encodeUtf8 <$> mp))
            if isValid
                then lift . setCreds True $ Creds "hash" (fromMaybe "" mu) []
                else loginErrorMessage LoginR "Invalid username/password"

            where
                validateUser userID passwd = do
                    user <- runDB $ getBy userID
                    case user of
                        Nothing -> return False
                        Just (Entity _ m) -> return $ verifyPassword passwd (userPassword m)

    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
