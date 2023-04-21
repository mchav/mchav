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
module PostManagement
    ( main
    ) where
import Yesod
import Database.Persist.Sqlite
import Control.Monad
import Data.List (intercalate)
import Data.Time
import Database.Persist.Sqlite
import System.Directory
import System.Environment
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post
    title String
    contents String
    created UTCTime default=CURRENT_TIME
    Title title
    deriving Show
|]

openConnectionCount :: Int
openConnectionCount = 10

postDirectory :: String
postDirectory = "./posts"

postPath :: String -> String
postPath file = postDirectory ++ "/" ++ file

removeNonPosts :: [String] -> [String]
removeNonPosts = filter ((==) ".post" . reverse . take 5. reverse)

main :: IO ()
main = do
    print "Updating blog..."
    files <- liftM removeNonPosts (getDirectoryContents postDirectory)
    print files
    forM_ files $ \file -> do
        (date : title : contents) <- liftM lines (readFile (postPath file))
        runStderrLoggingT $ withSqlitePool "mchav.db3" openConnectionCount $ \pool -> liftIO $ do
            runResourceT $ flip runSqlPool pool $ do
                runMigration migrateAll
                let time = parseTimeOrError True defaultTimeLocale "%Y/%m/%d" date :: UTCTime
                p <- getBy $ Title title
                case p of
                    Just (Entity postId post) -> update postId [PostContents =. (intercalate "\n" contents)]
                    Nothing                   -> insert (Post title (intercalate "\n" contents) time) >> return ()
