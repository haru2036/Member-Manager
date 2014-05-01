module Handler.UserConfirmation where

import Import

getUserConfirmationR :: Handler Html
getUserConfirmationR = do
  users <- runDB $ selectList [] [Desc UserIdent]
  defaultLayout $(widgetFile "user-confirmation")

postUserConfirmationR :: Handler Html
postUserConfirmationR = error "Not yet implemented: postUserConfirmationR"

