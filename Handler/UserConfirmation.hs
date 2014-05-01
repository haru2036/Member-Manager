module Handler.UserConfirmation where

import Import

getUserConfirmationR :: Handler Html
getUserConfirmationR = do
  users <- runDB $ selectList [] [Desc UserIdent]
  defaultLayout $(widgetFile "userList")

postUserConfirmationR :: Handler Html
postUserConfirmationR = error "Not yet implemented: postUserConfirmationR"

