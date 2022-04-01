{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Config                                (Config (..),
                                                        Environment (..),
                                                        setLogger)
import           Control.Monad.IO.Class                (liftIO)
import           DB                                    (Connection (..),
                                                        connStrFromConnection,
                                                        makePool)
import           Migration                             (doRunMigration)
import           Network.Wai.Handler.Warp              (run)
import           Network.Wai.Middleware.Servant.Errors (errorMw)
import           Safe                                  (readMay)
import           Servant.API.ContentTypes              (JSON)
import           System.Environment                    (lookupEnv)
import           UsersRouter                           (app')


lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing ->
      return def
    Just val ->
      maybe (handleFailedRead val) return (readMay val)
  where
    handleFailedRead val =
      error $ mconcat
        [
          "Failed to process '"
          , val
          , "' for env variable "
          , env
        ]

main :: IO ()
main = do
  putStrLn "Booting up..."
  env <- lookupSetting "ENV" Development
  port <- lookupSetting "PORT" 4040
  pool <- makePool env
  let config = Config { configPool = pool }
      logger = setLogger env
      connection = Connection "sndr" "postgres" "postgres"
      connStr = connStrFromConnection connection env
  doRunMigration config env
  run port $ logger $ errorMw @JSON @'["error", "status"] $ app' config
