{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-} 
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON SiteArticle where
    toJSON p = object ["slug"          .= siteArticleSlug p
                      ,"title"         .= siteArticleTitle p
                      ,"description"   .= siteArticleDescription p
                      ,"body"          .= siteArticleBody p
                      ,"tagList"       .= siteArticleTagList p
                      ,"createdAt"     .= siteArticleCreatedAt p
                      ,"updatedAt"     .= siteArticleUpdatedAt p
                      ,"favoriteCount" .= siteArticleFavoriteCount p]
