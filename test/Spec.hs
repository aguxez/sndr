{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

import           Data.List                            (head)
import           Database.Persist.Class.PersistEntity (Entity (..), Key)
import           Database.Persist.Class.PersistUnique (getBy)
import           Network.HTTP.Client                  (defaultManagerSettings,
                                                       newManager)
import qualified Network.Wai.Handler.Warp             as Warp

import           Config                               (Config (..),
                                                       Environment (..))
import           DB                                   (makePool, makeTestPool,
                                                       runDBNoTx)
import           Database.Persist.Sql.Migration       (runMigration)
import           Migration                            (doRunMigration,
                                                       dropAndCreateDB)
import           Servant
import           Servant.Client                       (BaseUrl (baseUrlPort),
                                                       ClientM, client,
                                                       mkClientEnv,
                                                       parseBaseUrl, runClientM)
import           Test.Hspec                           (Spec, around, describe,
                                                       hspec, it, runIO,
                                                       shouldBe)
import           User                                 (NewUserPayload (..),
                                                       User (..), UserId)
import           UsersRouter                          (UserAPI, app')

allUsers :: ClientM [Entity User]
createUser :: NewUserPayload -> ClientM (Entity User)
getUser :: UserId -> ClientM (Entity User)

allUsers :<|> getUser :<|> createUser = client (Proxy :: Proxy UserAPI)

withUserApp :: Config -> (Warp.Port -> IO ()) -> IO ()
withUserApp config = Warp.testWithApplication (pure (app' config))

businessLogicSpec :: Config -> Spec
businessLogicSpec config = around (withUserApp config) $ do
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

  -- tests
  describe "POST /users" $ do
    it "should create a user" $ \port -> do
      result <- runClientM (createUser (NewUserPayload { newUsername = "name1" })) (clientEnv port)
      let (Right entity) = result
      entityVal entity `shouldBe` User { userUsername = "name1" }

  describe "GET /users" $ do
    it "return all users" $ \port -> do
      result <- runClientM allUsers (clientEnv port)
      let (Right (entity:_)) = result
      entityVal entity `shouldBe` User { userUsername = "name1" }

  describe "GET /users/:userId" $ do
    it "should return a single user" $ \port -> do
      newUser <- runClientM (createUser (NewUserPayload { newUsername = "name2" })) (clientEnv port)
      let (Right newEntity) = newUser
      result <- runClientM (getUser (entityKey newEntity)) (clientEnv port)
      let (Right entity) = result
      entityKey entity `shouldBe` entityKey newEntity

spec :: Config -> Spec
spec config = do
  businessLogicSpec config

main :: IO ()
main = do
  testDbPool <- makeTestPool
  let testConfig = Config { configPool = testDbPool }
  dropAndCreateDB testConfig
  dbPool <- makePool Test
  let config = Config { configPool = dbPool }
  doRunMigration config
  hspec $ spec config
