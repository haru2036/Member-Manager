module Handler.ConfirmMember where

import Import

getConfirmMemberR :: String -> Handler Html
getConfirmMemberR code = do
  maybeUCMember <- runDB $ getBy $ UniqueConfirm code
  case maybeUCMember of
    Just uCMember -> confirmMember $ entityVal uCMember
    Nothing -> lift $ return()
  render <- getMessageRender
  let mayBeMessage = Just MsgMemberConfirmed
  defaultLayout $(widgetFile "info")

shrinkUCMember :: UnConfirmedMember -> Member
shrinkUCMember member = unConfirmedMemberMember member

confirmMember uCMember = do
  _ <- runDB $ deleteBy $ UniqueConfirm $ unConfirmedMemberConfirmKey (uCMember :: UnConfirmedMember)
  _ <- runDB $ insert $ shrinkUCMember uCMember
  lift $ return ()

