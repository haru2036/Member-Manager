{-# LANGUAGE OverloadedStrings #-}
module Handler.SendMail where

import Data.List 
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Network.Mail.Client.Gmail
import Network.Mail.Mime
import Import
import Helper.SendMail
import Data.Text.Encoding

getSendMailR :: Handler Html
getSendMailR = do
  affiliations <- runDB $ selectList [] [Desc AffiliationName]
  defaultLayout $(widgetFile "sendMail")

postSendMailR :: Handler Html
postSendMailR = do
  affiliation <- runInputPost $ ireq textField "affiliation"
  maybeAffiliation <- getAffiliation affiliation
  render <- getMessageRender
  eitherLink <- sendMailLinkToAffiliation maybeAffiliation 
  case eitherLink of
    Left msg -> do
      let maybeMessage = Just msg
      let maybeLink = (Nothing :: Maybe Text)
      defaultLayout $(widgetFile "infoMail")
    Right link -> do
      let maybeMessage = Nothing
      let maybeLink = Just link
      defaultLayout $(widgetFile "infoMail")

sendMailLinkToAffiliation (Just aff) = do
  maybeSender <- getSender
  affiliations <- getAffiliations
  members <- getMembers (inSubSeq (entityVal aff) (map (entityVal) affiliations))
  lift $ return $ Right $ sendMailUrl members
sendMailLinkToAffiliation Nothing = lift $ return $ Left MsgMailSendError

getAffiliation text = runDB $ getBy $ UniqueAffiliation text

getMembers inAffiliations = runDB $ selectList [MemberAffiliations <-. inAffiliations] [Asc MemberEmailAddress]

sendMailUrl :: [Entity Member] -> Text
sendMailUrl members = foldl (\acc x -> acc `T.append` "," `T.append` x) "https://mail.google.com/mail/?view=cm&fs=1&bcc=" $ map (memberEmailAddress . entityVal) members

inLst :: Eq a => a -> [[a]] -> [[a]]
inLst ina allList = filter (\x -> elem ina x) allList

inSubSeq :: Eq a => a -> [a] -> [[a]]
inSubSeq a aList = inLst a $ subsequences aList

