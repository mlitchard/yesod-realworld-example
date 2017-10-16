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
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Jasmine         (minifym)
import qualified Data.ByteString.Lazy as BL

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import Yesod.Default.Config hiding (appRoot, appExtra)
import Network.Wai.Internal as Wai 
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

-- | Response Bodies

-- | User
-- {
--   "user": {
--     "email": "jake@jake.jake",
--     "token": "jwt.token.here",
--     "username": "jake",
--     "bio": "I work at statefarm",
--     "image": null
--   }
-- }

type EmailAddress  = Text
type Username      = Text
type Slug          = Text
type Cid           = Int


toUserJSON :: Profile -> EmailAddress -> Username  -> Value
toUserJSON (Profile{..}) address  username = object ["user" .= user]
  where
    user = object [ "email"    .= address
                  , "username" .= username
                  , "bio"      .= profileBio
                  , "image"    .= profilePictureURL
                  ]

-- | Profile
-- {
--   "profile": {
--     "username": "jake",
--     "bio": "I work at statefarm",
--     "image": "https://static.productionready.io/images/smiley-cyrus.jpg",
--     "following": false
--   }
-- }

type ProfileJSON = Value
toProfileJSON :: Profile -> Username -> Bool -> ProfileJSON
toProfileJSON (Profile{..}) username following = object ["profile" .= profile]
  where
    profile = object [ "username"  .= username
                     , "bio"       .= profileBio
                     , "image"     .= profilePictureURL
                     , "following" .= following
                     ]

-- | Single Article
-- {
--   "article": {
--     "slug": "how-to-train-your-dragon",
--     "title": "How to train your dragon",
--     "description": "Ever wonder how?",
--     "body": "It takes a Jacobian",
--     "tagList": ["dragons", "training"],
--     "createdAt": "2016-02-18T03:22:56.637Z",
--     "updatedAt": "2016-02-18T03:48:35.824Z",
--     "favorited": false,
--     "favoritesCount": 0,
--     "author": {
--       "username": "jake",
--       "bio": "I work at statefarm",
--       "image": "https://i.stack.imgur.com/xHWG8.jpg",
--       "following": false
--     }
--   }
-- }

toArticlesJSON :: [(SiteArticleJSON,ProfileJSON)] -> Value
toArticlesJSON articleProfilePairs = object [ "articles" .= apJSON
                                            , "articlesCount" .= length articleProfilePairs]
  where
    apJSON = map (toArticleJSON T.empty) articleProfilePairs

type Label = Text
type SiteArticleJSON = Value

toArticleJSON :: Label -> (SiteArticleJSON,ProfileJSON) -> Value
toArticleJSON label (Object aObject, (Object pObject)) =
  if (label == T.empty) then nolabel else labeled
  where
    nolabel = Object $ aObject `H.union` pToAuthor
    pToAuthor = H.fromList $ map (\kv@(name,profile) -> if name == ("profile" :: Text)
                                                        then (("author" :: Text),profile)
                                                        else kv) $ H.toList pObject
    labeled = object ["article" .= article]
    article = aObject `H.union` pToAuthor
toArticleJSON _ _ = error ("error: toArticleJSON malformed json")

-- |
-- {
--  "comments": [{
--    "id": 1,
--    "createdAt": "2016-02-18T03:22:56.637Z",
--    "updatedAt": "2016-02-18T03:22:56.637Z",
--    "body": "It takes a Jacobian",
--    "author": {
--      "username": "jake",
--      "bio": "I work at statefarm",
--      "image": "https://i.stack.imgur.com/xHWG8.jpg",
--      "following": false
--    }
--  }]
-- }
type SiteCommentJSON = Value

toCommentsJSON :: [(SiteCommentJSON,ProfileJSON)] -> Value
toCommentsJSON commentProfilePairs = object [ "comments" .= cpJSON ]
  where
    cpJSON = map (toCommentJSON T.empty) commentProfilePairs

toCommentJSON :: Label -> (SiteCommentJSON,ProfileJSON) -> Value
toCommentJSON label (Object cObject, Object pObject) =
  if (label == T.empty) then comment else labeled
  where
    comment = Object $ cObject `H.union` pObject
    labeled = object ["comment" .= comment]
toCommentJSON _ _ = error ("error: toCommentJSON malformed json")    

data UserError
  = BadAuth
  | UserNotFound
  | NameTaken
  | EmailTaken
  deriving Show

data ArticleError
  = ArticleNotFound
  | ArticleNotAllowed
  deriving Show

data CommentError
  = CommentNotFound
  | SlugNotFound
  | CommentNotAllowed
  deriving Show

data Error
  = CommentErrorWrapper CommentError
  | UserErrorWrapper UserError
  | ArticleErrorWrapper ArticleError
  deriving Show

instance ToJSON Error where      
  toJSON (CommentErrorWrapper cerr) =
    let errmsg :: Text
        errmsg  = case cerr of
          CommentNotFound   -> "comment not found"
          SlugNotFound      -> "slug not found"
          CommentNotAllowed -> "comment not allowed"
    in toJSON errmsg      
  toJSON (UserErrorWrapper uerr)    =
    let errmsg :: Text
        errmsg = case uerr of
          BadAuth      -> "bad authorization"
          UserNotFound -> "user not found"
          NameTaken    -> "name already taken"
          EmailTaken   -> "email address already taken"
    in toJSON errmsg      
  toJSON (ArticleErrorWrapper aerr) =
    let errmsg :: Text
        errmsg = case aerr of
          ArticleNotFound   -> "article not found"
          ArticleNotAllowed -> "article not allowed"
    in toJSON errmsg      

toErrorsJSON :: [Error] -> Value
toErrorsJSON errs = object [ "errors" .= object [ "body" .= errmsgsJSON]]
  where
    errmsgsJSON = toJSON <$> errs
    
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

data UserAuthReq = UserAuthReq
  { email :: Text
  , password :: Text
  } deriving Show

instance FromJSON UserAuthReq where
  parseJSON = withObject "user" $ \o -> do
    authUser <- o .: "user"
    email    <- authUser .: "email"
    password <- authUser .: "password"
    return UserAuthReq {..}

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
  { username_ru :: Text
  , user_ru :: UserAuth
  , bio_ru  :: Maybe Text
  , image_ru :: Maybe Text
  } deriving Show

instance FromJSON RegUser where
  parseJSON = withObject "RegUser" $ \o -> do
    regUser  <- o .: "user"
    username_ru <- regUser .: "username"
    email       <- regUser .: "email"
    password    <- regUser .: "password"
    bio_ru      <- regUser .:? "bio"
    image_ru    <- regUser .:? "image"
    let user = UserAuth email password
    return $ RegUser username_ru user bio_ru image_ru 

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

-- | Response Bodies
-- SiteUser is generated with Template Haskell see config/models
                        
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
{-
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings
    authRoute _ = Just AuthErrorR
    isAuthorized AuthErrorR _          = return Authorized
    isAuthorized (StaticR _) _         = return Authorized
    isAuthorized FaviconR _            = return Authorized
    isAuthorized RobotsR _             = return Authorized
    isAuthorized (IntakeR _) _         = return Authorized
    isAuthorized (InterpreterR _) _    = return Authorized
    isAuthorized (ConnectClientR _) _  = return Authorized
    isAuthorized (MWIMainlineR _) _    = return Authorized
    isAuthorized _ _                   = httpBasicAuth
-}
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

getExtra :: Handler Extra
getExtra = do
  master <- getYesod
  return $ appExtra $ appSettings master
--  fmap (appExtra . appSettings) getYesod

httpBasicAuth :: HandlerT App IO AuthResult
httpBasicAuth = do
  request <- waiRequest
  conf    <- getExtra
  return $ case lookup "Authorization" (Wai.requestHeaders request) of
             Just x  -> if (x == encodedPlaceholder conf)
                        then Authorized
                        else AuthenticationRequired
             Nothing -> AuthenticationRequired
  where
    encodedPlaceholder conf = TE.encodeUtf8 $ extraBasicAuthPlaceholder conf
