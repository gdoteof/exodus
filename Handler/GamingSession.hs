{-# LANGUAGE NoMonomorphismRestriction #-}
module Handler.GamingSession
    ( postGamingSessionsR
    , getGamingSessionsR
    , getGamingSessionR
    , postGamingSessionR
    , postGamingSessionCloseR
    )
where

import Import
import Data.Time.Clock
import Data.Time.Format
import Database.Persist.Store
import Data.Text hiding (null, map)
import Data.Maybe
import System.Locale (defaultTimeLocale)
import Helpers.Model
import qualified Data.Map as Map
import Data.List as List hiding (insert)

postGamingSessionsR :: Handler RepHtml
postGamingSessionsR = do
    start <- liftIO $ getCurrentTime
    gs <- runInputPost $ GamingSession
                start
                Nothing
                <$> (textToKey <$> (ireq textField "player"))
                <*> (textToKey <$> (ireq textField "table"))
                <*> iopt intField "seat"
    let _ = gs :: GamingSession
    gsId <- runDB $ insert gs
    let startTime = formatTime defaultTimeLocale "%H:%M:%S (%b/%e/%y" (gamingSessionStart gs)
    player <- runDB (get404 (gamingSessionPlayer gs))
    table <- runDB (get404 (gamingSessionTable gs))
    defaultLayout $(widgetFile "newSession")
  
--textToKey = Key . PersistText . read . unpack
textToKey a = fromJust . fromPathPiece $ a

getGamingSessionsR :: Handler RepHtml
getGamingSessionsR = do
  {-records <- runDB $ do
    sessions <- selectList [GamingSessionEnd ==. Nothing] [Desc GamingSessionTable]
    let pids = map (gamingSessionPlayer . entityVal) sessions
    players <- selectList [PlayerId <-. pids] []
  --}
  records <- runDB $ do
      sessions <- selectList [GamingSessionEnd ==. Nothing] []
      players  <- selectList [] []
      tables   <- selectList [] []

      return $ joinTables3 gamingSessionPlayer gamingSessionTable sessions players tables

  let takenSeats = tableMeta records
  defaultLayout $(widgetFile ("opensessions"))

-- Generates a list of (Entty Table, [Int]) which represent the 'taken' seats for the 
tableMeta :: [(Entity GamingSession, Entity Player, Entity Table)] -> [(Entity Table, [Int])]
tableMeta [] = []
tableMeta xs = Import.foldl addUpdate [] xs

addUpdate :: [(Entity Table, [Int])] -> (Entity GamingSession, Entity Player, Entity Table) -> [(Entity Table, [Int])] 
addUpdate acc (g,p,table) = map update acc
   where 
     update ts@(t,s) | t==table = (t, List.sort(s++seat)) | otherwise = ts
     seat = (case (gamingSessionSeat (entityVal g)) of
                    Nothing -> []
                    _ -> [fromJust $ gamingSessionSeat $ entityVal g])

getGamingSessionR :: GamingSessionId -> Handler RepHtml
getGamingSessionR gamingSessionId = do 
     -- session <- runDB (get404 gamingSessionId)
    defaultLayout [whamlet|Get <h1>{#show session}|]

postGamingSessionR :: GamingSessionId -> Handler RepHtml
postGamingSessionR gamingSessionId = do 
    session <- runDB (get404 gamingSessionId)
    defaultLayout [whamlet|Post<h1>{#show session}|]

postGamingSessionCloseR :: GamingSessionId -> Handler RepHtml
postGamingSessionCloseR sid= do
    session <- runDB (get404 sid)
    person <- runDB (get404 $ gamingSessionPlayer session)
    end <- liftIO $ getCurrentTime
    runDB $ update  sid [GamingSessionEnd =. Just end]
    runDB $ update (gamingSessionPlayer session) [PlayerMinutes +=. Just (fromIntegral ( round (diffUTCTime end (gamingSessionStart session) )))]
    defaultLayout [whamlet|Session closed!|]

gamingSessionWidget :: GamingSessionId -> Player -> Table -> Widget
gamingSessionWidget sid p t = do
    let session = toPathPiece sid
    buttonId <- lift $ newIdent
    containerId <- lift $ newIdent
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
    toWidget[julius|
      $('##{buttonId}').click(function(){
         $.ajax({
            type: 'POST',
            url:'@{GamingSessionCloseR sid}', 
            success: function(data){ $('##{containerId}').remove();  },
            error: function(jqxhr,textStatus,errorThrown){ alert(textStatus + ': ' + errorThrown); },
            dataType: "html"
         } );
        
      });

    |]
    $(widgetFile "gamingSession/_session_row")



