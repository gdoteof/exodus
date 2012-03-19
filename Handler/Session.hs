module Handler.GamingSession
    ( postGamingSessionR
    , getGamingSessionsR
    )
where

postGamingSessionR :: Handler RepHtml
postGamingSessionR = do
    gs <- runInputPost $ GamingSession
                Nothing
                <$> ireq textField "player"
                <*> ireq textField "table"
                <*> ireq intField "seat"
    defaultLayout [] 
  
getGamingSessionR :: Handler RepHtml
getGamingSessionR = do
  defaultLayout [whamlet |<h1>Hi|]
