module Handler.ConfirmMember where

import Import

getConfirmMemberR :: String -> Handler Html
getConfirmMemberR code = do
  maybeUCMember <- runDB $ getBy $ UniqueConfirm code
  let maybeMember = shrinkMaybeUCMember maybeUCMember
  case maybeMember of
    Just member -> insertMember member
    Nothing -> lift $ return ()
  render <- getMessageRender
  let mayBeMessage = Just MsgMemberConfirmed
  defaultLayout $(widgetFile "info")

shrinkMaybeUCMember :: Maybe (Entity UnConfirmedMember) -> Maybe Member
shrinkMaybeUCMember (Just member) = Just $ shrinkUCMember $ entityVal member
shrinkMaybeUCMember Nothing = Nothing

shrinkUCMember :: UnConfirmedMember -> Member
shrinkUCMember member = unConfirmedMemberMember member

--insertMember :: Member -> HandlerT site IO ()
insertMember member = do 
  _ <- runDB $ insert member
  lift $ return ()
