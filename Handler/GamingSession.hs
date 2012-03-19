module Handler.GamingSession
    ( postGamingSessionR
    , getGamingSessionR
    )
where

import Import
import Data.Time.Clock
import Database.Persist.Store
import Data.Text
import Data.Maybe
import Safe (readMay)

postGamingSessionR :: Handler RepHtml
postGamingSessionR = do
    start <- liftIO $ getCurrentTime
    (player, table, seat) <- runInputPost $ (,,)
                <$> (ireq textField "player")
                <*> (ireq textField "table")
                <*> iopt intField "seat"
    playerId <- maybe (invalidArgs ["couldn't parse: ", player]) return $ readMay $ unpack player
    tableId <- maybe (invalidArgs ["couldn't parse: ", table]) return $ readMay $ unpack table
    let gs = GamingSession start Nothing playerId tableId seat 
    gsId <- runDB $ insert gs
    defaultLayout $(widgetFile "newSession.hamlet")
  
--textToKey = Key . PersistText . read . unpack
--textToKey = fromJust .  fromPathPiece


getGamingSessionR :: Handler RepHtml
getGamingSessionR = do
  defaultLayout [whamlet|
<form action=@{GamingSessionR} method=post>
    <p>
        player
        <input type=text name=player value=4f6150251c21230c78000000>
        table
        <input type=text name=table value=4f651bfa1c21231873000000>
        table
        <input type=text name=seat value=2>
        <input type=submit value="make">
|]
