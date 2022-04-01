module Config (Config (..), Environment (..), setLogger) where

import           Database.Persist.Sql                 (ConnectionPool)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

newtype Config = Config { configPool :: ConnectionPool }

data Environment = Test | Development deriving (Show, Read)

setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
