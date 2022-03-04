{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module DB (runDB, uuidDef) where

import           Config                                  (Config (..))
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Reader                    (MonadIO)
import qualified Data.ByteString.Char8                   as BS8
import qualified Data.UUID                               as UUID
import           Database.Persist.ImplicitIdDef          (ImplicitIdDef)
import           Database.Persist.ImplicitIdDef.Internal (mkImplicitIdDef)
import           Database.Persist.PersistValue           (LiteralType (Escaped),
                                                          PersistValue (PersistLiteral_))
import           Database.Persist.Sql                    (PersistField (..),
                                                          PersistFieldSql (..),
                                                          SqlPersistT,
                                                          SqlType (SqlOther),
                                                          runSqlPool)
import           Web.PathPieces                          (PathPiece (..),
                                                          readFromPathPiece,
                                                          showToPathPiece)

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
runDB ::(MonadIO m) => Config -> SqlPersistT IO a -> m a
runDB conf action = liftIO $ runSqlPool action (configPool conf)

uuidDef :: ImplicitIdDef
uuidDef = mkImplicitIdDef @UUID.UUID "uuid_generate_v4()"
