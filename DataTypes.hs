module DataTypes where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Aeson

import SexEnum

data ResponderInfo =
  ResponderInfo {
    entranceName :: String,
    userName :: String,
    userOpenId :: String
  } | ResponderInfoParseFailure
    deriving (Eq, Show, Read)

instance FromJSON ResponderInfo where
  parseJSON (Object v) = ResponderInfo <$>
    v .: "entranceName" <*>
    v .: "userName" <*>
    v .: "userOpenId"
  parseJSON _ = mempty

-- Application routing/database/resource configuration, no modification unless
-- you are extremely certain what they're and what are their function.
data HsacnuLockControl =
  HsacnuLockControl {
    servPort :: Int,
    servDomain :: String,
    wcAppId :: String,
    dbName :: String,
    dbConnPoolNum :: Int,
    templateMsgId :: String,
    responders :: [ResponderInfo]
  } | HsacnuLockControlParseFailure
    deriving (Eq, Show, Read) -- | HsacnuLockControl
-- $(makeLenses ''HsacnuLockControlWithConfig)

instance FromJSON HsacnuLockControl where
  parseJSON (Object v) = HsacnuLockControl <$>
    v .: "servPort" <*>
    v .: "servDomain" <*>
    v .: "wcAppId" <*>
    v .: "dbName" <*>
    v .: "dbConnPoolNum" <*>
    v .: "templateMsgId" <*>
    v .: "responders"
  parseJSON _ = mempty

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  UserInfo
    openId String
    nickName String
    sex Sex Maybe
    province String Maybe
    city String Maybe
    country String Maybe
    headImageUrl String Maybe
    privilege String Maybe
    unionId String
    Primary openId
    deriving Show Read Eq
|]

{-
-- UserInfo Primary ADT
data UserInfo =
  RequesterUser {
    openId :: String,
    nickName :: String,
    sex :: Sex,
    province :: String,
    city :: String,
    country :: String,
    headImageUrl :: String,
    privilege :: [String],
    unionId :: String
  } | ResponderUser {
    placeholder :: () -- TODO Repsonder
  } deriving (Eq, Show, Read)
-}

