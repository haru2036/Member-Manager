module Helper.SendMail where

import Import

getSender = runDB $ selectFirst [] [Asc SenderName]

getAffiliationList = do 
  aflist <- runDB $ selectList [] [Desc AffiliationName]
  return $ map (\x -> (affiliationName (entityVal x), entityVal x)) $ aflist

getAffiliations = runDB $ selectList [] [Desc AffiliationName]
