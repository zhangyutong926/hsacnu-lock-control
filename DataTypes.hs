module DataTypes where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Aeson

data ResponderInfo =
  ResponderInfo {
    responderId :: Int,
    entranceName :: String,
    userName :: String,
    userOpenId :: String
  } deriving (Eq, Show, Read)

instance FromJSON ResponderInfo where
  parseJSON (Object v) = ResponderInfo <$>
    v .: "responderId" <*>
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
    callbackDomain :: String,
    wcAppId :: String,
    wcAppSecret :: String,
    dbName :: String,
    dbConnPoolNum :: Int,
    templateMsgId :: String,
    responders :: [ResponderInfo],
    languagePreference :: String,
    siteName :: String
  } deriving (Eq, Show, Read)

instance FromJSON HsacnuLockControlConf where
  parseJSON (Object v) = HsacnuLockControlConf <$>
    v .: "servPort" <*>
    v .: "servDomain" <*>
    v .: "callbackDomain" <*>
    v .: "wcAppId" <*>
    v .: "wcAppSecret" <*>
    v .: "dbName" <*>
    v .: "dbConnPoolNum" <*>
    v .: "templateMsgId" <*>
    v .: "responders" <*>
    v .: "languagePreference" <*>
    v .: "siteName"
  parseJSON _ = mempty

data HsacnuLockControl =
  HsacnuLockControl {
    appConf :: HsacnuLockControlConf,
    connPool :: ConnectionPool
  }

data GetAccessTokenResponse =
  GetAccessTokenResponse {
    accessToken :: String,
    openId :: String
  } deriving (Eq, Show, Read)

instance FromJSON GetAccessTokenResponse where
  parseJSON (Object v) = GetAccessTokenResponse <$>
    v .: "access_token" <*>
    v .: "openid"
  parseJSON _ = mempty

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  UserInfo
    openId String
    nickName String
    sex Int
    language String
    city String
    province String
    country String
    headImageUrl String
    privilege [String]
    Primary openId
    deriving Show Read Eq
|]

instance FromJSON UserInfo where
  parseJSON (Object v) = UserInfo <$>
    v .: "openid" <*>
    v .: "nickname" <*>
    v .: "sex" <*>
    v .: "language" <*>
    v .: "city" <*>
    v .: "province" <*>
    v .: "country" <*>
    v .: "headimgurl" <*>
    v .: "privilege"
  parseJSON _ = mempty
