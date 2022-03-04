{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module UsersRouter (app') where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Config                               (Config)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           DB                                   (runDB)
import           Data.UUID                            (UUID)
import           Database.Persist.Class.PersistEntity (Entity)
import           Database.Persist.Class.PersistQuery  (selectList)
import           Database.Persist.Class.PersistStore  (getEntity, insertEntity)
import           Database.Persist.Sql                 (SqlPersistT)
import           Network.Wai                          (Application)
import           Servant
import           Servant.Server                       (ServerError (..), err404)
import           Servant.Server.Internal.Handler      (Handler)
import           User                                 (NewUserPayload (..),
                                                       User (..), UserId)

type UserAPI = "users" :>
                (
                     Get '[JSON] [Entity User]
                :<|> Capture "userId" UserId :> Get '[JSON] (Entity User)
                :<|> ReqBody '[JSON] NewUserPayload :> Post '[JSON] (Entity User)
                )

userServer :: Config -> Server UserAPI
userServer conf = allUsers :<|> getUser :<|> createUser
  where
    -- Gets all users
    allUsers :: MonadIO m => m [Entity User]
    allUsers = liftIO $ runDB conf $ selectList [] []

    -- Get a single user
    getUser :: UserId -> Handler (Entity User)
    getUser userId = do
      maybeUser <- liftIO $ runDB conf $ getEntity userId
      case maybeUser of
        Just v  -> return v
        Nothing -> throwError $ err404 { errBody = BSL.pack "user not found" }

    -- Creates an user and returns itself
    createUser :: MonadIO m => NewUserPayload -> m (Entity User)
    createUser newUser = liftIO $ runDB conf $ insertEntity (User (newUsername newUser))

app' :: Config -> Application
app' conf = serve (Proxy :: Proxy UserAPI) (userServer conf)
