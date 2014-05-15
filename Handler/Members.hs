module Handler.Members where

import Import

getMembersR :: Handler Html
getMembersR = do
  members <- runDB $ selectList [] [Desc MemberFirstName]
  defaultLayout $(widgetFile "members")
