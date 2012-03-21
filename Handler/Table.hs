module Handler.Table
    ( getTablesR
    , postTablesR
    , getTableR
    , tableCheckinWidget
    )
where


import Import
import Helpers.Model

tableForm :: Form Table
tableForm = renderDivs $ Table
    <$> areq   textField     "Name"             Nothing
    <*> areq   textField     "Game"             Nothing
    <*> areq   intField      "Points per hour"  Nothing
    <*> aopt   intField      "Number of Seats"  Nothing
    <*> aopt   textField     "Description"      Nothing


getTablesR :: Handler RepHtml
getTablesR = do
    defaultLayout $ do
        [whamlet|<h1>table list|]
    tables <- runDB $ selectList [] [Desc TableName]
    -- We'll need the two "objects": articleWidget and enctype
    -- to construct the form (see templates/articles.hamlet).
    ((_,tableWidget), enctype) <- generateFormPost tableForm
    defaultLayout $ do
        setTitle "Tables"
        $(widgetFile "tables")


postTablesR :: Handler RepHtml
postTablesR = do
    ((res,tableWidget),enctype) <- runFormPost tableForm
    case res of 
         FormSuccess table -> do 
            tableId <- runDB $ insert table
            setMessage $ toHtml $ (tableName table) <> " created"
            redirect $ TableR tableId 
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "tableAddError")


getTableR :: TableId ->  Handler RepHtml
getTableR tableId = do
     table <- runDB (get404 tableId)
     defaultLayout $ do 
                   setTitle ( "Tables")
                   $(widgetFile "table")


tableCheckinWidget :: [Entity Table] -> Widget
tableCheckinWidget tableList = do
     let tables = map (addIdent .entityVal) tableList
     addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
     $(widgetFile "tableCheckinWidget")


addIdent a = do
  identity <- lift newIdent
  return (identity, a)
