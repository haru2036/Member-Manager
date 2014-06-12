module Handler.SendMail where

import Data.List 
import qualified Data.Text.Lazy as LT
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
  title       <- runInputPost $ ireq textField "title"
  body        <- runInputPost $ ireq textField "body"
  maybeAffiliation <- getAffiliation affiliation
  render <- getMessageRender
  msg <- sendMailToAffiliation maybeAffiliation title body
  let mayBeMessage = Just msg
  defaultLayout $(widgetFile "info")

sendMailToAffiliation (Just aff) title body = do
  maybeSender <- getSender
  affiliations <- getAffiliations
  members <- getMembers (inSubSeq (entityVal aff) (map (entityVal) affiliations))
  case maybeSender of
    Just sender -> do
      _ <- lift $ sendMail (entityVal sender) title body members
      lift $ return MsgMailSent
    Nothing -> lift $ return MsgSenderNotFoundError
sendMailToAffiliation Nothing _ _ = lift $ return MsgMailSendError

getAffiliation text = runDB $ getBy $ UniqueAffiliation text

getMembers inAffiliations = runDB $ selectList [MemberAffiliations <-. inAffiliations] [Asc MemberEmailAddress]

sendMail :: Sender -> Text -> Text -> [Entity Member] -> IO ()
sendMail sender title body members = do
                            print (map (\x -> (memberEmailAddress (entityVal x))) members)
                            sendGmail 
                               (LT.fromStrict (senderGmail sender)) 
                               (LT.fromStrict (senderPasswd sender)) 
                               (Address (Just (senderName sender)) 
                               (senderGmail sender)) 
                               [] 
                               [] 
                               (map (\x -> Address (Just (memberEmailAddress (entityVal x))) (memberEmailAddress (entityVal x))) members)
                               (title) 
                               (LT.fromStrict body)
                               []

inLst :: Eq a => a -> [[a]] -> [[a]]
inLst ina allList = filter (\x -> elem ina x) allList

inSubSeq :: Eq a => a -> [a] -> [[a]]
inSubSeq a aList = inLst a $ subsequences aList

