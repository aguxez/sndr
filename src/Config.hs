{-# LANGUAGE OverloadedStrings          #-}

module Config (makePool, Config (..), Environment (..), setLogger) where

import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Trans.Reader           (ReaderT)
import qualified Data.ByteString                      as BS (ByteString)
import qualified Data.ByteString.Char8                as BS8 (append, pack)
import           Database.Persist.Postgresql          (ConnectionString,
                                                       createPostgresqlPool)
import           Database.Persist.Sql                 (ConnectionPool)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant                              (Handler (..))
import           Servant.Server.Internal.ServerError  (ServerError)

newtype Config = Config { configPool :: ConnectionPool }

data Environment = Test | Development deriving (Read)

setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev

connStr :: String -> ConnectionString
connStr suffix = BS8.pack $ "dbname=sndr" <> suffix <> " user=postgres password=postgres"

envPool :: Environment -> Int
envPool Test        = 5
envPool Development = 5

makePool :: Environment -> IO ConnectionPool
makePool Test = runNoLoggingT (createPostgresqlPool (connStr "_test") (envPool Test))
makePool Development = runStdoutLoggingT (createPostgresqlPool (connStr "_dev") (envPool Development))
