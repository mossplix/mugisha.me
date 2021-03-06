module Handler.Root where

import           Data.List (find)
import           Import
import           Yesod.Markdown
getRootR :: Handler Html
getRootR = do
  posts <- runDB $ select $ from $ \e -> do
                   orderBy [desc (e ^. EntryDate)]
                   limit 6
                   return e
  cats <- runDB $ select $ from $ \c -> do
                  orderBy [asc (c ^. CategoryName)]
                  return c
  defaultLayout $ do
    setTitle "Moses Mugisha"
    $(widgetFile "root")
  where findCat p cs = find (\ c -> entityKey c == entryCat p) cs

nofacebookme_png :: StaticRoute
nofacebookme_png = StaticRoute ["no-facebook-me.png"] []

haskeller_logo :: StaticRoute
haskeller_logo = StaticRoute ["haskeller.png"] []

ilovefs :: StaticRoute
ilovefs = StaticRoute ["ilovefs-banner-extralarge.png"] []
