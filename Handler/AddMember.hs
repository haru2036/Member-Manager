module Handler.AddMember where

import Import
import Data.Hash
import Data.Word
import qualified Data.Text as T
import Network.SMTP.Simple

getAddMemberR :: Handler Html
getAddMemberR = do 
  affiliations <- getAffiliationList
  (widget, enctype) <- generateFormPost (addMemberForm affiliations)
  defaultLayout $(widgetFile "memberAdd")

postAddMemberR :: Handler Html
postAddMemberR = do
  affiliations <- getAffiliationList
  ((result, widget), enctype) <- runFormPost (addMemberForm affiliations)
  affiliations <- getAffiliationList
  defaultLayout $(widgetFile "memberAdd")

sendConfirmMail member = do
  let address = memberEmailAddress member
  let hash = hashText address 
  lift $ sendMail hash address
  runDB $ insert UnConfirmedMember
                    { unConfirmedMemberConfirmKey = hash
                    , unConfirmedMemberMember = member
                    }

sendMail :: String -> Text -> IO()
sendMail code address = do
  sendSimpleMessages (\x -> putStrLn x) "smtp.gmail.com" "gmail.com" [message]
       where message = SimpleMessage
                          [NameAddr Nothing "team@exmaple.com"]
                          [NameAddr Nothing (T.unpack address)]
                          "アカウントの検証"
                          ("ソフトウェア研究部からメールアドレスの検証をお願いします。あなたが登録したものなら以下のURLにアクセスしてください : " ++ (show code))

hashText :: Text -> String
hashText text = show $ asWord64 $ hash $ T.unpack text

addMemberForm :: [(Text, Affiliation)] ->  Html -> MForm Handler (FormResult Member, Widget)
addMemberForm affiliations = do 
  renderBootstrap $ Member
    <$> areq textField "姓" Nothing
    <*> areq textField "名" Nothing
    <*> areq intField  "入学年(下二桁)" Nothing
    <*> areq textField  "学科記号" Nothing
    <*> areq intField  "学籍番号(下3桁)" Nothing
    <*> areq textField "eMailアドレス" Nothing
    <*> areq (multiSelectFieldList (affiliations)) "所属する班" Nothing

getAffiliationList = do 
  aflist <- runDB $ selectList [] [Desc AffiliationName]
  return $ map (\x -> (affiliationName (entityVal x), entityVal x)) $ aflist

