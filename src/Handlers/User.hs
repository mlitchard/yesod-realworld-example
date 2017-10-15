{-# LANGUAGE OverloadedStrings #-}
module Handlers.User where

import Import
-- HandlerT App IO (Maybe (Entity record0))


postAuthR :: Handler Value
postAuthR = error ("postAuthR undefined")

postUsersR :: Handler Value
postUsersR = error ("postUserR undefined")

getUserR :: Handler Value
getUserR = error ("getUserR")

putUserR :: Handler Value
putUserR = error ("putUser undefined")

getProfileR :: Username -> Handler Value
getProfileR = error ("getProfile undefined")

postFollowR :: Username -> Handler Value
postFollowR = error ("follow undefined")

deleteFollowR :: Username -> Handler Value
deleteFollowR = error ("delete follow undefined")


