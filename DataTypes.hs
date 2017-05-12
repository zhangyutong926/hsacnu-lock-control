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
  } deriving (Eq, Show, Read)

instance FromJSON ResponderInfo where
  parseJSON (Object v) = ResponderInfo <$>
    v .: "entranceName" <*>
    v .: "userName" <*>
    v .: "userOpenId"
  parseJSON _ = mempty

-- Application routing/database/resource configuration, no modification unless
-- you are extremely certain what they're and what are their function.
data HsacnuLockControlConf =
  HsacnuLockControlConf {
    servPort :: Int,
    servDomain :: String,
    wcAppId :: String,
    wcAppSecret :: String,
    dbName :: String,
    dbConnPoolNum :: Int,
    templateMsgId :: String,
    responders :: [ResponderInfo]
  } deriving (Eq, Show, Read)

data HsacnuLockControl =
  HsacnuLockControl {
    appConf :: HsacnuLockControlConf,
    connPool :: ConnectionPool
  }

instance FromJSON HsacnuLockControlConf where
  parseJSON (Object v) = HsacnuLockControlConf <$>
    v .: "servPort" <*>
    v .: "servDomain" <*>
    v .: "wcAppId" <*>
    v .: "wcAppSecret" <*>
    v .: "dbName" <*>
    v .: "dbConnPoolNum" <*>
    v .: "templateMsgId" <*>
    v .: "responders"
  parseJSON _ = mempty

data GetAccessTokenResponse =
  GetAccessTokenResponse {
    accessToken :: String,
    openId :: String
  } deriving (Eq, Show, Read)

instance FromJSON GetAccessTokenResponse where
  parseJSON (Object v) = GetAccessTokenResponse <$>
    v .: "access_token" <*>
    v .: "openId"
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
