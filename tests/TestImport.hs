{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module X
    , runDB
    , Spec
    , Example
    ) where

import Yesod.Test as X
import Database.Persist as X hiding (get)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)
import Control.Monad.IO.Class (liftIO)

import Foundation as X
import Model as X

type Spec = YesodSpec App
type Example = YesodExample App

runDB :: SqlPersistM a -> Example a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runSqlPersistMPool query pool
