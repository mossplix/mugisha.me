{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    , StaticRoute
    , AuthRoute
    , requireAdmin
    , maybeAdmin
    ) where

import qualified Database.Persist
import           Database.Persist.Sql (SqlPersistT)
import           Model
import           Network.HTTP.Conduit (Manager)
import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Hamlet (hamletFile)
import Yesod.Fay
import Yesod.Core.Types (Logger)


-- Custom imports
import           Control.Applicative
import           Control.Arrow        ((&&&))
import           Data.Text            (Text)
import           Data.Time
import           System.Locale
import           Text.Blaze           (ToMarkup (..))
import           Text.Blaze.Internal  (preEscapedText)
import           Yesod.AtomFeed

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
    , fayCommandHandler :: CommandHandler App
    , appLogger :: Logger
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- TODO Swap this function into a utility module.
plural :: Int -> String -> String -> String
plural 1 x _ = x
plural _ _ y = y

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
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuth
        cats <- runDB $ selectList [] [Asc CategoryName]
        currentRoute <- getCurrentRoute
        let isCurrent x = (currentRoute == Just x) || ((parents currentRoute) == Just x)
        let categories = map (name &&& EntriesR . name) cats

        -- static links to images
        let powered_by_logo = StaticRoute ["powered_by_yesod.png"] []
        let rss_logo = StaticRoute ["rss_logo.png"] []

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                ])
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
        where name = categoryName . entityVal

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
        addStaticContentExternal Right genFileName Settings.staticDir (StaticR . flip StaticRoute [])
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

instance YesodJquery App
instance YesodFay App where

    fayRoute = FaySiteR

    yesodFayCommand render command = do
        master <- getYesod
        fayCommandHandler master render command

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB f = do
        master <- getYesod
        Database.Persist.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR


    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing Nothing False

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId def , authGoogleEmail]

    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod


instance ToMarkup UTCTime where
    toMarkup = toMarkup . formatTime defaultTimeLocale "%e.%m.%Y"

instance ToMessage UTCTime where
    toMessage = toMessage . formatTime defaultTimeLocale "%e.%m.%Y"

parents :: Maybe (Route App) -> Maybe (Route App)
parents (Just ImpressumR) = Nothing
parents (Just (EntriesByTagR cat _)) = Just $ EntriesR cat
parents (Just (EntryR cat _)) = Just $ EntriesR cat
parents (Just (NewEntryR cat)) = Just $ EntriesR cat
parents (Just (UploadFileR cat _)) = Just $ EntriesR cat
parents (Just (EntryCommentR cat _ _)) = Just $ EntriesR cat
parents (Just (EditEntryR cat _)) = Just $ EntriesR cat
parents (Just _) = Nothing
parents Nothing = Nothing

requireAdmin :: Handler ()
requireAdmin = do
     (Entity _ u) <- requireAuth
     if userAdmin u
       then return ()
       else permissionDenied "You need admin privileges to do that"

maybeAdmin :: Handler (Maybe User)
maybeAdmin = do
     mu <- maybeAuth
     case mu of
          Just (Entity _ u) -> do
            if userAdmin u
              then return $ Just u
              else return Nothing
          Nothing -> return Nothing

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
