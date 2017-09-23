{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Foundation where

import Import.NoFoundation

import Data.Aeson
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Jasmine         (minifym)


import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

-- | Request Bodies

-- | Authentication
-- POST /api/users/login
-- {
--    "user":{
--      "email": "jake@jake.jake",
--      "password": "jakejake"
--    }
-- }
-- Required fields: email, password

data User = User
  { email    :: Text
  , password :: Text
  } deriving Show

instance FromJSON User where
  parseJSON = withObject "user" $ \o -> do
    authUser <- o .: "user"
    email    <- authUser .: "email"
    password <- authUser .: "password"
    return User{..}

-- | Registration
-- POST /api/users
-- {
--    "user":{
--      "username": "Jacob",
--      "email": "jake@jake.jake",
--      "password": "jakejake"
--    }
-- }
-- Required fields: email, username, password

data RegUser = RegUser
  { username :: Text
  , user :: User
  } deriving Show

instance FromJSON RegUser where
  parseJSON = withObject "RegUser" $ \o -> do
    regUser  <- o .: "user"
    username <- regUser .: "username"
    email    <- regUser .: "email"
    password <- regUser .: "password"
    let user = User email password
    return $ RegUser username user 

-- | Update User
-- PUT /api/user
-- {
--    "user":{
--      "email": "jake@jake.jake",
--      "bio": "I like to skateboard",
--      "image": "https://i.stack.imgur.com/xHWG8.jpg"
--    }
-- }
-- Accepted fields: email, username, password, image, bio

data UpdateUser = UpdateUser
  { email_m    :: Maybe Text
  , bio_m      :: Maybe Text
  , image_m    :: Maybe Text
  , username_m :: Maybe Text
  , password_m :: Maybe Text
  } deriving Show

instance FromJSON UpdateUser where
  parseJSON = withObject "UpdateUser" $ \o -> do
    userUpdate <- o .: "user"
    email_m    <- (Just <$> userUpdate .: "email")    <|> pure Nothing
    bio_m      <- (Just <$> userUpdate .: "bio")      <|> pure Nothing
    image_m    <- (Just <$> userUpdate .: "image")    <|> pure Nothing
    username_m <- (Just <$> userUpdate .: "username") <|> pure Nothing
    password_m <- (Just <$> userUpdate .: "password") <|> pure Nothing
    return UpdateUser{..}

-- | Create Article
-- POST /api/articles
-- {
--   "article": {
--     "title": "How to train your dragon",
--     "description": "Ever wonder how?",
--     "body": "You have to believe",
--     "tagList": ["reactjs", "angularjs", "dragons"]
--   }
-- }
-- Required fields: title, description, body
-- Optional fields: tagList as an array of Strings

data NewArticle = NewArticle
  { title_ca       :: Text
  , desc_ca :: Text
  , body_ca        :: Text
  , tagList_ca_m   :: Maybe [Text]
  } deriving Show

instance FromJSON NewArticle where
  parseJSON = withObject "CreateArticle" $ \o -> do
    article      <- o .: "article"
    title_ca     <- article .: "title"
    desc_ca      <- article .: "description"
    body_ca      <- article .: "body"
    tagList_ca_m <- (Just <$> article .: "tagList") <|> pure Nothing
    return NewArticle{..}

-- | Update Article
-- PUT /api/articles/:slug
-- {
--    "article": {
--      "title": "Did you train your dragon?"
--    }
-- }
-- The slug also gets updated when the title is changed

data ArticleUpdate = ArticleUpdate
  { title_au :: Maybe Text
  , desc_au  :: Maybe Text
  , body_au  :: Maybe Text
  , slug_au  :: Maybe Text
  } deriving Show

instance FromJSON ArticleUpdate where
  parseJSON = withObject "ArticleUpdate" $ \o -> do
    articleUpdate <- o .: "article"
    title_au      <- (Just <$> articleUpdate .: "title")       <|> pure Nothing
    desc_au       <- (Just <$> articleUpdate .: "description") <|> pure Nothing 
    body_au       <- (Just <$> articleUpdate .: "body")        <|> pure Nothing
    slug_au       <- (Just <$> articleUpdate .: "slug")        <|> pure Nothing
    return ArticleUpdate{..}

-- | Add Comments to article
-- POST /api/articles/:slug/comments
-- {
--   "comment": {
--     "body": "His name was my name too."
--   }
-- }
-- Required fields: body

newtype Comment = Comment {commentBody :: Text} deriving Show

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    comment <- o .: "comment"
    commentBody <- comment .: "body"
    return Comment{..}

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool


-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
