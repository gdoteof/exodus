{-# LANGUAGE TypeSynonymInstances, OverloadedStrings #-}
module Handler.Table
    ( getTablesR
    , postTablesR , getTableR
    , tableCheckinWidget
    )
where


import Import
import Helpers.Model
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import qualified Data.Text.Lazy as TL
import Text.Julius
import qualified Data.Text as T hiding (null)
import Data.Maybe
import Database.Persist.Store
import qualified Data.Text as T
import Debug.Trace

tableForm :: Form Table
tableForm = renderDivs $ Table
    <$> areq   textField     "Name"             Nothing
    <*> areq   textField     "Game"             (Just "NL Holdem $1/$2")
    <*> areq   intField      "Points per hour"  (Just 1)
    <*> areq   intField      "Number of Seats"  (Just 9)
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


tableCheckinWidget :: PlayerId -> Widget
tableCheckinWidget playerId= do
     tables <- lift $ runDB $ selectList [] []
     tableTuple <-  lift $ mapM addIdent tables
     addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
     $(widgetFile "tableCheckinWidget")

addIdent :: Entity Table -> Handler (Text, TableId, Table)
addIdent (Entity tableId table) = do
  identity <- newIdent
  return (identity,  tableId, table)

tableClickHandlerWidget :: String -> TableId -> PlayerId -> Maybe Int ->  Widget
tableClickHandlerWidget elemId tableId playerId seatId = do
  let seatNumber = if seatId == Nothing 
                    then "null" 
                    else show  $ fromJust seatId
  liftIO $ print seatNumber
  let pid = toPathPiece $ playerId
  let tid = toPathPiece $ tableId
  toWidget[julius|
        $('##{elemId}').click(function(){
            $.ajax({
              type: 'POST',
              url:'@{GamingSessionsR}', 
              data: { player: #{show pid}, table: #{show tid}, seat: #{seatNumber} },
              success: function(data){ alert('success'); console.log(data);},
              error: function(jqxhr,textStatus,errorThrown){ alert(textStatus + ': ' + errorThrown); },
           } );
            
        });
      |]

