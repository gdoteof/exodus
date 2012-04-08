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
    <*> areq   intField "Minutes to Start" (Just 0)
    <*> pure   False

getPlayerListR :: Handler RepHtml
getPlayerListR = do
    players <- runDB $ selectList [] [Desc PlayerName]
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
     let minutes =  playerMinutes player
     defaultLayout $ do 
                   setTitle "Testing" 
                   $(widgetFile "player")
