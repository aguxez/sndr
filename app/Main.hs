{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config                   (Config (..), Environment (..), makePool, setLogger)
import           Network.Wai.Handler.Warp (run)
import           Safe                     (readMay)
import           System.Environment       (lookupEnv)
import           UsersRouter              (app')

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
  run port $ logger $ app' config
