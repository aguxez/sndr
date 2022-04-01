{-# LANGUAGE OverloadedStrings #-}

module Migration (doRunMigration, dropAndCreateDB) where

import           Config                         (Config)
import           Control.Monad.IO.Class         (MonadIO)
import           DB                             (runDB, runDBNoTx)
import           Database.Persist.Sql           (rawExecute)
import           Database.Persist.Sql.Migration (Migration, runMigration,
                                                 runSqlCommand)
import           User                           (migrateAll)

dropTestDB :: Migration
dropTestDB = runSqlCommand $ rawExecute "DROP DATABASE IF EXISTS sndr_test;" []

createTestDB :: Migration
createTestDB = runSqlCommand $ rawExecute "CREATE DATABASE sndr_test;" []

doRunMigration :: MonadIO m => Config -> m ()
doRunMigration config = runDB config (runMigration migrateAll)

dropAndCreateDB :: MonadIO m => Config -> m ()
dropAndCreateDB config = do
  runDBNoTx config (runMigration dropTestDB)
  runDBNoTx config (runMigration createTestDB)
