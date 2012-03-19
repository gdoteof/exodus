{-# LANGUAGE NoMonomorphismRestriction #-}
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

postGamingSessionR :: Handler RepHtml
postGamingSessionR = do
    start <- liftIO $ getCurrentTime
    gs <- runInputPost $ GamingSession
                start
                Nothing
                <$> (textToKey <$> (ireq textField "player"))
                <*> (textToKey <$> (ireq textField "table"))
                <*> iopt intField "seat"
    let _ = gs :: GamingSession
    gsId <- runDB $ insert gs
    defaultLayout $(widgetFile "newSession")
  
--textToKey = Key . PersistText . read . unpack
textToKey a = fromJust . fromPathPiece $ a

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
