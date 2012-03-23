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
  -}
  records <- runDB $ do
      sessions <- selectList [GamingSessionEnd ==. Nothing] []
      players  <- selectList [] []
      tables   <- selectList [] []

      return $ joinTables3 gamingSessionPlayer gamingSessionTable sessions players tables

  defaultLayout $(widgetFile ("opensessions"))

  

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
    $(widgetFile "gamingSession/_session_row")
