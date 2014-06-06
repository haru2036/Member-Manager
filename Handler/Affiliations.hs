module Handler.Affiliations where

import Import

getAffiliationsR :: Handler Html
getAffiliationsR = do
  items <- runDB $ selectList [] [Desc AffiliationName]
  let lists = $(widgetFile "affiliationsRow")
  let frm = $(widgetFile "affiliationForm")
  let maybeForm = Just frm
  defaultLayout $(widgetFile "list")

postAffiliationsR :: Handler Html
postAffiliationsR = do
  affiliationPost<- runInputPost $ ireq textField "affiliationName"
  addremove <- runInputPost $ ireq textField "addremove"
  case addremove of
    "add"-> addAffiliation affiliationPost
    "remove" -> removeAffiliation affiliationPost
    _ -> lift $ return ()
  items <- runDB $ selectList [] [Desc AffiliationName]
  let lists = $(widgetFile "affiliationsRow")
  let frm = $(widgetFile "affiliationForm")
  let maybeForm = Just frm
  defaultLayout $(widgetFile "list")

addAffiliation name = do 
  runDB $ insert Affiliation
                    { affiliationName = name}
  return ()

removeAffiliation name = runDB $ deleteBy $ UniqueAffiliation name

