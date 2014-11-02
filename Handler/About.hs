module Handler.About where

import Import

getAboutR :: Handler Html
getAboutR = do
    defaultLayout $ do
            aDomId <- newIdent
            setTitle "Moses Mugisha"
            $(widgetFile "homepage")
