module Handler.Blog where
import           Data.List (find)

import Import

getBlogR :: Handler Html
getBlogR = do
    posts <- runDB $ select $ from $ \e -> do
                   orderBy [desc (e ^. EntryDate)]
                   limit 5
                   return e
    cats <- runDB $ select $ from $ \c -> do
                  orderBy [asc (c ^. CategoryName)]
                  return c
    defaultLayout $ do
        setTitle "Moses Mugisha"
        $(widgetFile "blog")
      where findCat p cs = find (\ c -> entityKey c == entryCat p) cs
