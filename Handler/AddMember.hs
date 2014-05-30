{-# LANGUAGE OverloadedStrings #-}
module Handler.AddMember where

import Import
import Data.Hash
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.Mail.Client.Gmail
import Network.Mail.Mime

getAddMemberR :: Handler Html
getAddMemberR = do 
  affiliations <- getAffiliationList
  (widget, enctype) <- generateFormPost (addMemberForm affiliations)
  defaultLayout $(widgetFile "memberAdd")

postAddMemberR :: Handler Html
postAddMemberR = do
  affiliations <- getAffiliationList
  ((result, widget), enctype) <- runFormPost (addMemberForm affiliations)
  render <- getMessageRender
  mayBeMessage <- case result of
                    FormSuccess member -> sendConfirmMail member
                    _ -> lift $ return $ Just MsgFormError
  defaultLayout $(widgetFile "info")

sendConfirmMail member = do
  let address = memberEmailAddress member
  let hash = hashText address 
  sender <- getSender
  render <- getUrlRender
  case sender of
    Just sndr -> do 
      lift $ sendMail (entityVal sndr) hash address render
      _ <- runDB $ insert UnConfirmedMember
                        { unConfirmedMemberConfirmKey = hash
                        , unConfirmedMemberMember = member
                        }
      lift $ return (Just MsgUserPending)
    Nothing -> lift $ return (Just MsgSenderNotFoundError)


getSender = runDB $ selectFirst [] [Asc SenderName]

sendMail sender code address render = sendGmail 
                               (LT.fromStrict (senderGmail sender)) 
                               (LT.fromStrict (senderPasswd sender)) 
                               (Address (Just (senderName sender)) 
                               (senderGmail sender)) 
                               [Address Nothing address] 
                               [] 
                               [] 
                               (T.pack ("ソフ研から、アカウントの認証のお願い")) 
                               (LT.append (LT.pack "このコードを使ってアカウントを認証してください。") (LT.fromStrict (buildCodeMessage code render)))
                               []

buildCodeMessage code render = render (ConfirmMemberR code)

hashText :: Text -> String
hashText text = show $ asWord64 $ hash $ T.unpack text

addMemberForm :: [(Text, Affiliation)] ->  Html -> MForm Handler (FormResult Member, Widget)
addMemberForm affiliations extra = do 
  (firstNameResult, firstNameView) <- mreq textField "姓" Nothing
  (secondNameResult, secondNameView) <- mreq textField "名" Nothing
  (phoneResult, phoneView) <- mreq textField "電話番号" Nothing
  (gradResult, gradView) <- mreq intField  "入学年(下二桁)" Nothing
  (departmentResult, departmentView) <- mreq textField  "学科記号" Nothing
  (numberResult, numberView) <- mreq intField  "学籍番号(下3桁)" Nothing
  (mailResult, mailView) <- mreq emailField "eMailアドレス" Nothing
  (affiliationResult, affiliationView) <- mreq (multiSelectFieldList (affiliations)) "所属する班" Nothing
  (paidResult, paidView) <- mreq alwaysFalseField "" (Just False)
  let result = Member
          <$> firstNameResult
          <*> secondNameResult
          <*> phoneResult
          <*> gradResult
          <*> departmentResult
          <*> numberResult
          <*> mailResult
          <*> affiliationResult
          <*> paidResult
      widget = $(widgetFile "addMemberForm")
  return (result, widget)

alwaysFalseField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Bool
alwaysFalseField = Field
      { fieldParse = \_ _ -> return $ Right $ Just False
      , fieldView = \_ _ _ _ _ -> [whamlet| |]
      , fieldEnctype = UrlEncoded
                                    }


getAffiliationList = do 
  aflist <- runDB $ selectList [] [Desc AffiliationName]
  return $ map (\x -> (affiliationName (entityVal x), entityVal x)) $ aflist

