module Handler.UserConfirmation where

import Import


getUserListR :: Handler Html
getUserListR = do
  users <- runDB $ selectList [] [Desc UserIdent]
  defaultLayout $(widgetFile "userList")

getUserConfirmationR :: Text -> Handler Html
getUserConfirmationR userName = do
  render <- getMessageRender
  mayBeUser <- runDB $ getBy $ UniqueUser userName
  let mayBeMessage = getConfirmedMessage Nothing mayBeUser
  defaultLayout $(widgetFile "userConfirmForm")

postUserConfirmationR :: Text -> Handler Html
postUserConfirmationR userName = do
  posted <- runInputPost $ ireq checkBoxField "isConfirmed"
  render <- getMessageRender 
  mayBeCurrentUser <- runDB $ getBy $ UniqueUser userName
  let mayBeMessage = getConfirmedMessage (Just posted) mayBeCurrentUser
  case mayBeCurrentUser of
    Just user -> runDB $ update (entityKey user) [UserIsConfirmed =. posted]
  mayBeUser <- runDB $ getBy $ UniqueUser userName
  defaultLayout $(widgetFile "userConfirmForm")

getConfirmedMessage :: Maybe Bool -> Maybe a -> Maybe AppMessage
getConfirmedMessage (Just True) (Just _) = Just MsgUserConfirmed
getConfirmedMessage (Just False) (Just _) = Just MsgUserIgnored
getConfirmedMessage Nothing (Just _) = Nothing
getConfirmedMessage _ Nothing = Just MsgUserNotFound
