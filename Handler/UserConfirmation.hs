module Handler.UserConfirmation where

import Import


getUserListR :: Handler Html
getUserListR = do
  users <- runDB $ selectList [] [Desc UserIdent]
  defaultLayout $(widgetFile "userList")

getUserConfirmationR :: Handler Html
getUserConfirmationR = error "Not yet implemented: getUserConfirmationR"

postUserConfirmationR :: Handler Html
postUserConfirmationR = error "Not yet implemented: postUserConfirmationR"

