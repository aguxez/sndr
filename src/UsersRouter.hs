{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module UsersRouter (app') where

import           Config                               (Config)
import           Control.Monad.IO.Class               (liftIO)
import           DB                                   (runDB)
import           Data.Aeson                           (ToJSON)
import           Database.Persist.Class.PersistEntity (Entity)
import           Database.Persist.Class.PersistQuery  (selectList)
import           Database.Persist.Class.PersistStore  (insertEntity)
import           Database.Persist.Sql                 (SqlPersistT)
import           Network.Wai                          (Application)
import           Servant
import           Servant.Server.Internal.ServerError  (ServerError)
import           User                                 (NewUserPayload (..),
                                                       User (..))

type UserAPI = "users" :> Get '[JSON] [Entity User]
                :<|> "users" :> ReqBody '[JSON] NewUserPayload :> Post '[JSON] (Entity User)

userServer :: Config -> Server UserAPI
userServer conf = allUsers :<|> createUser
  where
    -- Gets all users
    allUsers = liftIO $ runDB conf $ selectList [] []

    -- Creates an user and returns itself
    createUser newUser = liftIO $ runDB conf $ insertEntity (User (newUsername newUser))

app' :: Config -> Application
app' conf = serve (Proxy :: Proxy UserAPI) (userServer conf)
