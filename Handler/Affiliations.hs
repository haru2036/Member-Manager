module Handler.Affiliations where

import Import

getAffiliationsR :: Handler Html
getAffiliationsR = do
  affiliations <- runDB $ selectList [] [Desc AffiliationName]
  defaultLayout $(widgetFile "affiliationsList")

postAffiliationsR :: Handler Html
postAffiliationsR = do
  affiliationPost<- runInputPost $ ireq textField "affiliationName"
  addremove <- runInputPost $ ireq textField "addremove"
  case addremove of
    "add"-> addAffiliation affiliationPost
    "remove" -> removeAffiliation affiliationPost
    _ -> lift $ return ()
  affiliations <- runDB $ selectList [] [Desc AffiliationName]
  defaultLayout $(widgetFile "affiliationsList")

addAffiliation name = do 
  runDB $ insert Affiliation
                    { affiliationName = name}
  return ()

removeAffiliation name = runDB $ deleteBy $ UniqueAffiliation name

