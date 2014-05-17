module Handler.AddMember where

import Import

getAddMemberR :: Handler Html
getAddMemberR = do 
  affiliations <- getAffiliationList
  (widget, enctype) <- generateFormPost (addMemberForm affiliations)
  defaultLayout $(widgetFile "memberAdd")

postAddMemberR :: Handler Html
postAddMemberR = error "Not yet implemented: postAddMemberR"

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

