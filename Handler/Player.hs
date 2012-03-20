module Handler.Player
    ( getPlayerListR
    , postPlayerListR
    , getPlayerR
    )
where


import Import 

-- to use Html into forms
import Yesod.Form.Nic (YesodNic, nicHtmlField)
import Data.Maybe
import Handler.Table
import Helpers.Model

playerForm :: Form Player
playerForm = renderDivs $ Player
    <$> areq   textField "Name" Nothing
    <*> areq   textField "Nick" Nothing
    <*> aopt   textField "Email" Nothing
    <*> aopt   textField "Phone" Nothing
    <*> aopt   textareaField "Notes" Nothing
    <*> aopt   intField "Minutes to Start" Nothing
    <*> pure   False

getPlayerListR :: Handler RepHtml
getPlayerListR = do
    defaultLayout $ do
        [whamlet|<h1>iget|]
    players <- runDB $ selectList [] [Desc PlayerName]
    -- We'll need the two "objects": articleWidget and enctype
    -- to construct the form (see templates/articles.hamlet).
    ((_,playerWidget), enctype) <- generateFormPost playerForm
    defaultLayout $ do
        setTitle "Player List"
        $(widgetFile "players")


postPlayerListR :: Handler RepHtml
postPlayerListR = do
    ((res,playerWidget),enctype) <- runFormPost playerForm
    case res of 
         FormSuccess player -> do 
            playerId <- runDB $ insert player
            setMessage $ toHtml $ (playerName player) <> " created"
            redirect $ PlayerR playerId 
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "playerAddError")


getPlayerR :: PlayerId -> Handler RepHtml
getPlayerR playerId = do
     player <- runDB (get404 playerId)
     tables <- runDB $ selectList [] [Desc TableName]
     let minutes = if playerMinutes player == Nothing
                   then 0
                   else fromJust $ playerMinutes player
     defaultLayout $ do 
                   setTitle "Testing" 
                   $(widgetFile "player")
