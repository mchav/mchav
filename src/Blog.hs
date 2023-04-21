{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Blog
    ( main
    ) where
import Yesod
import Yesod.Markdown
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Time
import Text.Blaze.Html.Renderer.Pretty
import Data.Time.Format
import Data.Monoid
import Text.Lucius (luciusFile)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post
    title String
    contents String
    created UTCTime default=CURRENT_TIME
    Title title
    deriving Show
|]

data App = App ConnectionPool

mkYesod "App" [parseRoutes|
/ HomeR GET
/post/#PostId PostR GET
|]

instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

getHomeR :: Handler Html
getHomeR = do
    posts <- runDB $ selectList [] [Desc PostCreated]
    defaultLayout $ do
        toWidget $(luciusFile "templates/homepage.lucius")
        $(whamletFile "templates/homepage.hamlet")

getPostR :: PostId -> Handler Html
getPostR postId = do
    post <- runDB $ get404 postId
    defaultLayout $ do
        toWidget $(luciusFile "templates/post.lucius")
        let contents = (preEscapedToMarkup $ postContents post)
        [whamlet|
            <h2>#{postTitle post}
            ^{contents}
        |]
        

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "mchav.db3" 10 $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
    warp 3000 $ App pool
