module Handler.GamingSession
    ( postGamingSessionR
    , getGamingSessionsR
    )
where

postGamingSessionR :: Handler RepHtml
postGamingSessionR = do
    gs <- runInputPost $ GamingSession
                do { start <- getCurrentTime; start }
                Nothing
                <$> ireq textField "player"
                <*> ireq textField "table"
                <*> ireq intField "seat"
    defaultLayout [whamlet|<h1> posted |]
  
getGamingSessionR :: Handler RepHtml
getGamingSessionR = do
  defaultLayout [whamlet |<h1>Hi]
