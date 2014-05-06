module Handler.UserConfirmation where

import Import
import Data.Text


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
  -- ToDo: Append some codes to get the userIdent from Post Request
  mayBeUser <- runDB $ getBy $ UniqueUser userName
  -- ToDo: Append some codes to save to DB
  defaultLayout $(widgetFile "userConfirmForm")

