{-
-- Project Hsacnu Lock Control
-- Author: Yutong Zhang
-}

module Main where

import qualified Network.URI.Encode as URIE
import qualified Data.Text as ST
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Data.Aeson (decode, encode, eitherDecode, FromJSON, ToJSON, (.:))
import Control.Monad.Logger (runNoLoggingT)
import Data.String (fromString)
import Text.Blaze.Html (toHtml)
import System.Directory (doesFileExist, removeFile)

import Yesod.Core
import Yesod.Persist.Core
import Text.Hamlet
import Text.Shakespeare.Text
import Text.Blaze.Html.Renderer.String
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Network.HTTP.Simple

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Aeson
import Network.HTTP.Client

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
    siteName :: String,
    invalidationMilliSec :: Int
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
    v .: "siteName" <*>
    v .: "invalidationMilliSec"
  parseJSON _ = mempty

data HsacnuLockControl =
  HsacnuLockControl {
    appConf :: HsacnuLockControlConf,
    connPool :: ConnectionPool
  }

instance Yesod HsacnuLockControl where
  approot = ApprootMaster $ ST.pack . servDomain . appConf

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
    UserInfoOpenIdU openId
    deriving Show Read Eq
  UnlockReqInfo
    userOpenId String
    responderOpenId String
    issuedTime String
    invalidatedTime String
    Primary userOpenId
    deriving Show Read Eq
|]

instance YesodPersist HsacnuLockControl where
  type YesodPersistBackend HsacnuLockControl = SqlBackend

  runDB action = do
    HsacnuLockControl {..} <- getYesod
    runSqlPool action connPool

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

instance FromJSON UnlockReqInfo where
  parseJSON (Object v) = UnlockReqInfo <$>
    v .: "userOpenId" <*>
    v .: "responderOpenId" <*>
    v .: "issuedTime" <*>
    v .: "invalidatedTime"
  parseJSON _ = mempty

mkYesod "HsacnuLockControl" [parseRoutes|
/ HomeR GET
/wechat_openid_redirect WeChatOpenIDRedirectR GET
/wechat_openid_callback WeChatOpenIDCallbackR GET
/submit_request_and_wait SubmitRequestAndWaitR GET
|]

{-
/lock_pending LockPendingR GET
/refresh_lock_status RefreshLockStatus GET
/responder_respond ResponderRespondR GET
/responce_submit ResponceSubmitR GET
-}

-- Prepared jQuery outsite source
jQueryW :: Widget
jQueryW = addScriptRemote "https://code.jquery.com/jquery-3.2.1.min.js"

-- Handler is the controller of web application (controller in MVC)
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  app <- getYesod
  let HsacnuLockControlConf {..} = appConf app
  setTitle $ toHtml $ siteName ++ " - Home Page"
  toWidget [hamlet|
    <h1>#{siteName} - Home Page
    <p>
    <a href=@{WeChatOpenIDRedirectR}>WeChat OpenID Validation
  |]

-- Redirect the user to the WeChat OpenID Login Page
getWeChatOpenIDRedirectR :: Handler ()
getWeChatOpenIDRedirectR = do
  app <- getYesod
  let conf = appConf app
  urlRender <- getUrlRender
  let encoded = URIE.encode $ ST.unpack $ urlRender WeChatOpenIDCallbackR
  liftIO $ putStrLn encoded
  redirect ([st|
    https://open.weixin.qq.com/connect/oauth2/authorize?appid=#{wcAppId conf}&redirect_uri=#{encoded}&response_type=code&scope=snsapi_userinfo#wechat_redirect
  |])

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

getWeChatOpenIDCallbackR :: Handler Html
getWeChatOpenIDCallbackR = do
  wcOpenIdCode <- lookupGetParam "code"
  let Just code = wcOpenIdCode
  app <- getYesod
  let HsacnuLockControlConf {..} = appConf app
  let targetAccessToken = [st|https://api.weixin.qq.com/sns/oauth2/access_token?appid=#{wcAppId}&secret=#{wcAppSecret}&code=#{code}&grant_type=authorization_code|]
  requestAccessToken <- parseRequest $ ST.unpack targetAccessToken
  -- responseAccessTokenL8 <- httpLBS requestAccessToken
  -- let accessTokenResponseBody = getResponseBody responseAccessTokenL8
  -- lift $ print accessTokenResponseBody
  (responseAccessToken :: Response GetAccessTokenResponse) <- httpJSON requestAccessToken
  let accessTokenJSON = getResponseBody responseAccessToken
  let token = accessToken accessTokenJSON
  let userOpenId = openId accessTokenJSON
  let targetUserInfo = [st|https://api.weixin.qq.com/sns/userinfo?access_token=#{token}&openid=#{userOpenId}&lang=#{languagePreference}|]
  requestUserInfo <- parseRequest $ ST.unpack targetUserInfo
  -- responseUserInfoL8 <- httpLBS requestUserInfo
  -- let userInfoResponseBody = getResponseBody responseUserInfoL8
  -- lift $ print userInfoResponseBody
  (responseUserInfo :: Response UserInfo) <- httpJSON requestUserInfo
  let userInfo = getResponseBody responseUserInfo
  let UserInfo {..} = userInfo
  _ <- runDB $ deleteBy $ UserInfoOpenIdU (let UserInfo id _ _ _ _ _ _ _ _ = userInfo in id)
  openId <- runDB $ insert userInfo
  defaultLayout $ do
    setTitle $ toHtml $ siteName ++ " - Login Callback"
    toWidget [hamlet|
      <h1>Login successful!
      <p>
        <ul>
          $forall ResponderInfo responderId entrance name wcOpenId <- responders
            <li><a href=@?{(SubmitRequestAndWaitR, [("userId", ST.pack (show openId)),("responderId", ST.pack (show responderId)), ("accessToken", ST.pack (show token))])}>#{entrance}
    |]
  {- defaultLayout $ do
    setTitle "Debug"
    toWidget [hamlet|
      <h1>Debug Only!
      <p>#{show userInfoResponseBody}
    |] -}

data TemplatedMessageRequest =
  TemplatedMessageRequest {
    toUser :: String,
    templateId :: String,
    url :: String,
    topColor :: String
  } deriving (Eq, Show, Read)

instance ToJSON TemplatedMessageRequest where
  toJSON TemplatedMessageRequest {..} = object [
    "touser" .= toUser,
    "template_id" .= templateId,
    "url" .= url,
    "topcolor" .= topColor ]

getSubmitRequestAndWaitR :: Handler Html
getSubmitRequestAndWaitR = do
  Just userId <- lookupGetParam "userId"
  Just responderId <- lookupGetParam "responderId"
  Just accessToken <- lookupGetParam "accessToken"
  app <- getYesod
  let HsacnuLockControlConf {..} = appConf app
  let reqJson = encode $ TemplatedMessageRequest (ST.unpack responderId) templateMsgId ("javascript:alert(\"" ++ (ST.unpack userId) ++ "\\n" ++ (ST.unpack responderId) ++ "\\n" ++ (ST.unpack accessToken) ++ "\");window.location.replace('https://www.inria.fr/');") "#666"
  nakedRequest <- parseRequest $ "POST https://api.wechat.com/cgi-bin/message/template/send?access_token=" ++ (ST.unpack accessToken)
  let request = nakedRequest { method = "POST", requestBody = RequestBodyLBS reqJson }
  response <- httpLBS request
  defaultLayout $ toWidget [hamlet|#{L8.unpack $ getResponseBody response}|]

parseJsonConf :: BS.ByteString -> HsacnuLockControlConf
parseJsonConf src = case eitherDecode src of
  Right d -> d
  Left e -> error $ "ERROR: Failure during parsing the JSON config file, details in: \n" ++ e

jsonConfFileName :: FilePath
jsonConfFileName = "config.json"

readJsonFileAndParse :: IO HsacnuLockControlConf
readJsonFileAndParse = do
  fileContent <- BS.readFile jsonConfFileName
  return $ parseJsonConf fileContent

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = do
  exist <- doesFileExist fileName
  if exist
    then removeFile fileName
    else return ()
  return ()

main :: IO ()
main = do
  putStrLn "HsacnuLockControl Project Server-Side Software Version 1.0, initiating..."
  putStrLn "Source Code License: Public Domain License CC0"
  putStrLn "Academia Humana magna est."

  conf <- readJsonFileAndParse
  print conf -- FIXME Debug Only
  let HsacnuLockControlConf {..} = conf
  removeIfExists dbName
  pool <- runNoLoggingT $ createSqlitePool (ST.pack dbName) dbConnPoolNum
  runSqlPool (runMigration migrateAll) pool
  let appInst = HsacnuLockControl conf pool
  
  putStrLn "Haskell Yesod Warp Web Engine, initiating..."
  warp servPort appInst
