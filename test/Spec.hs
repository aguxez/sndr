{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

import Data.List (head)
import           Database.Persist.Class.PersistEntity (Entity (..))
import           Network.HTTP.Client                  hiding (Proxy)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp             as Warp

import           Config                               (Config (..),
                                                       Environment (..),
                                                       makePool)
import           Servant
import           Servant.Client
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher
import           User                                 (NewUserPayload (..),
                                                       User (..), UserId)
import           UsersRouter                          (UserAPI, app')

allUsers :: ClientM [Entity User]
createUser :: NewUserPayload -> ClientM (Entity User)

allUsers :<|> getUser :<|> createUser = client (Proxy :: Proxy UserAPI)

withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action = do
  dbPool <- makePool Test
  let config = Config { configPool = dbPool }
  Warp.testWithApplication (pure (app' config)) action

businessLogicSpec :: Spec
businessLogicSpec = around withUserApp $ do
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

  -- tests
  describe "POST /users" $ do
    it "should create a user" $ \port -> do
      result <- runClientM (createUser (NewUserPayload { newUsername = "some name" })) (clientEnv port)
      let (Right entity) = result
      entityVal entity `shouldBe` User { userUsername = "some name" }

  describe "GET /users" $ do
    it "return all users" $ \port -> do
      result <- runClientM allUsers (clientEnv port)
      let (Right (entity:_)) = result
      entityVal entity `shouldBe` User { userUsername = "some name" }

spec :: Spec
spec = do
  businessLogicSpec

main :: IO ()
main = hspec spec
