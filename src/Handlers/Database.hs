{-# LANGUAGE OverloadedStrings #-}
module Handlers.Database where

import Import
-- HandlerT App IO (Maybe (Entity record0))

testUserAuth :: Handler UserAuthId
testUserAuth = runDB $ insert $ UserAuth "bob" "bobpass"


testProfile :: UserAuthId -> Handler ProfileId
testProfile uid = runDB $ insert $ Profile uid (Just "bob likes haskell") Nothing 
  
getProfile :: UserAuthId -> Handler (Maybe Profile)
getProfile uid = do
  maybeProfile <- runDB $ getBy $ UniquePID uid
  case maybeProfile of
    Nothing -> return Nothing
    Just (Entity _ profile) -> return $ Just profile
  
testTable :: UserAuthId -> Handler (Maybe SiteArticle)
testTable uid = do
  cTime  <- lift $ getCurrentTime
  udTime <- lift $ getCurrentTime
  runDB $ do
    testID <- insert $ SiteArticle uid "slug" "title" "desc" "body" ["tag","list"] cTime udTime 5
    test   <- get testID                                                 
    return test 
