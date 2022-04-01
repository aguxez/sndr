{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DB (runDB, testConnStr, uuidDef, makeTestPool, Connection (..), connStrFromConnection, makePool, runDBNoTx) where

import           Config                                  (Config (..),
                                                          Environment (..))
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Logger                    (runNoLoggingT,
                                                          runStdoutLoggingT)
import           Control.Monad.Reader                    (MonadIO)
import qualified Data.ByteString.Char8                   as BS8
import qualified Data.UUID                               as UUID
import           Database.Persist.ImplicitIdDef          (ImplicitIdDef)
import           Database.Persist.ImplicitIdDef.Internal (mkImplicitIdDef)
import           Database.Persist.PersistValue           (LiteralType (Escaped),
                                                          PersistValue (PersistLiteral_))
import           Database.Persist.Postgresql             (ConnectionString,
                                                          createPostgresqlPool)
import           Database.Persist.Sql                    (ConnectionPool,
                                                          IsolationLevel (..),
                                                          PersistField (..),
                                                          PersistFieldSql (..),
                                                          SqlPersistT,
                                                          SqlType (SqlOther),
                                                          runSqlPool,
                                                          runSqlPoolNoTransaction)
import           Web.PathPieces                          (PathPiece (..),
                                                          readFromPathPiece,
                                                          showToPathPiece)


data Connection = Connection {
  connectionDbName     :: !String,
  connectionDbUser     :: !String,
  connectionDbPassword :: !String
} deriving (Show)

connStrFromConnection :: Environment -> ConnectionString
connStrFromConnection env = BS8.pack $ "dbname=sndr" <> suffix <> " user=postgres password=postgres"
  where suffix = case env of
          Development -> "_dev"
          Test        -> "_test"

testConnStr :: ConnectionString
testConnStr = BS8.pack "dbname=postgres user=postgres password=postgres"

instance PersistField UUID.UUID where
  toPersistValue = PersistLiteral_ Escaped . BS8.pack . UUID.toString

  fromPersistValue (PersistLiteral_ Escaped t) =
    case UUID.fromString $ BS8.unpack t of
      Just x  -> Right x
      Nothing -> Left "Invalid UUID"

  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID.UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID.UUID where
  toPathPiece = showToPathPiece
  fromPathPiece = readFromPathPiece

-- Simply runs a DB action
runDB :: (MonadIO m) => Config -> SqlPersistT IO a -> m a
runDB conf action = liftIO $ runSqlPool action (configPool conf)

-- Simply runs a DB action outside of a transaction
runDBNoTx :: (MonadIO m) => Config -> SqlPersistT IO a -> m a
runDBNoTx conf action = liftIO $ runSqlPoolNoTransaction action (configPool conf) (Just ReadCommitted)

uuidDef :: ImplicitIdDef
uuidDef = mkImplicitIdDef @UUID.UUID "gen_random_uuid()"

envPool :: Environment -> Int
envPool Test        = 5
envPool Development = 5

makePool :: Environment -> IO ConnectionPool
makePool env = do
  let connStr = connStrFromConnection env
  case env of
    Test -> runNoLoggingT (createPostgresqlPool connStr (envPool Test))
    Development -> runStdoutLoggingT (createPostgresqlPool connStr (envPool Development))

-- Creates a pool of a specific db on test so we can do actions without opening a database we'll be using.
-- For example, dropping a database is not quite supported if the database is open.
makeTestPool :: IO ConnectionPool
makeTestPool = runNoLoggingT (createPostgresqlPool testConnStr (envPool Test ))
