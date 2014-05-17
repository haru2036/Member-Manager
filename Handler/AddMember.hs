module Handler.AddMember where

import Import

getAddMemberR :: Handler Html
getAddMemberR = do 
  (widget, enctype) <- generateFormPost addMemberForm
  defaultLayout $(widgetFile "memberAdd")

postAddMemberR :: Handler Html
postAddMemberR = error "Not yet implemented: postAddMemberR"

addMemberForm :: Html -> MForm Handler (FormResult Member, Widget)
addMemberForm = do 
  aflist <- getAffiliationList
  renderDivs $ Member
    <$> areq textField "姓" Nothing
    <*> areq textField "名" Nothing
    <*> areq intField  "入学年" Nothing
    <*> areq textField  "学科記号" Nothing
    <*> areq intField  "学籍番号(下3桁)" Nothing
    <*> areq textField "eMailアドレス" Nothing
    <*> areq (multiSelectFieldList (aflist)) "チェックボックス(Bool)" Nothing

getAffiliationList = do 
  aflist <-runDB $ selectList [] [Desc AffiliationName]
  return $ map (\x -> (entityVal x, entityVal x)) $ aflist

