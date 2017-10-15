{-# LANGUAGE OverloadedStrings #-}
module Handlers.Article where
  

import Import

getArticlesR :: Handler Value
getArticlesR = error ("get articles undefined")

postArticlesR :: Handler Value
postArticlesR = error (" post articles undefined")

getFeedR :: Handler Value
getFeedR = error ("get feed undefined")

getArticleR :: Slug -> Handler Value
getArticleR = error ("get article undefined")

putArticleR :: Slug -> Handler Value
putArticleR = error ("put article undefined")

deleteArticleR :: Slug -> Handler Value
deleteArticleR = error ("delete article undefined")

postFavoriteR :: Slug -> Handler Value
postFavoriteR = error ("post favorite undefined")

deleteFavoriteR :: Slug -> Handler Value
deleteFavoriteR = error ("delete favorite undefined")

getTagsR :: Handler Value
getTagsR = error ("get tags undefined") 

