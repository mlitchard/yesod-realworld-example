{-# LANGUAGE OverloadedStrings #-}
module Handlers.Comments where

import Import

postCommentR :: Slug -> Handler Value
postCommentR = error ("add comment undefined")

getCommentR :: Slug -> Handler Value
getCommentR = error ("get comment undefined")

deleteACommentR :: Slug -> Cid -> Handler Value
deleteACommentR = error ("delete comment undefined")



