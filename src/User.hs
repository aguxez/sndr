{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module User where

import           DB                  (uuidDef)
import           Data.Aeson          (FromJSON, ToJSON)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      setImplicitIdDef, share, sqlSettings)
import           GHC.Generics        (Generic)

share [mkPersist (setImplicitIdDef uuidDef sqlSettings), mkMigrate "migrateAll"] [persistLowerCase|
  User json sql=users
    username   String

    UniqueUsername username
    
    deriving Show Eq
|]

newtype NewUserPayload = NewUserPayload { newUsername :: String } deriving (Generic)

instance FromJSON NewUserPayload

instance ToJSON NewUserPayload
