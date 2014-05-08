module Handler.UserConfirmation where

import Import


getUserListR :: Handler Html
getUserListR = do
  users <- runDB $ selectList [] [Desc UserIdent]
  defaultLayout $(widgetFile "userList")

getUserConfirmationR :: Text -> Handler Html
getUserConfirmationR userName = do
  mayBeUser <- runDB $ getBy $ UniqueUser userName
  defaultLayout $(widgetFile "userConfirmForm")

postUserConfirmationR :: Text -> Handler Html
postUserConfirmationR userName = do
  posted <- runInputPost $ ireq checkBoxField "isConfirmed"
  mayBeCurrentUser <- runDB $ getBy $ UniqueUser userName
  case mayBeCurrentUser of
    Just user -> runDB $ update (entityKey user) [UserIsConfirmed =. posted]
  mayBeUser <- runDB $ getBy $ UniqueUser userName
  defaultLayout $(widgetFile "userConfirmForm")

