module Handler.Projects where

import Import

getProjectsR :: Handler Html
getProjectsR = do
    defaultLayout $ do
            setTitle "Moses Mugisha"
            $(widgetFile "projects")

