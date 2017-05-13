{-
-- Project Hsacnu Lock Control
-- Author: Yutong Zhang
-}

module Main where

import qualified Network.URI.Encode as URIE
import qualified Data.Text as ST
import qualified Data.ByteString.Lazy as BS

import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Data.Aeson (decode, encode, eitherDecode, FromJSON, ToJSON, (.:))
import Control.Monad.Logger (runNoLoggingT)

import Yesod.Core
import Yesod.Persist.Core
import Text.Hamlet
import Text.Shakespeare.Text
import Text.Blaze.Html.Renderer.String
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Network.HTTP.Simple

import DataTypes

mkYesod "HsacnuLockControl" [parseRoutes|
/ HomeR GET
/wechat_openid_redirect WeChatOpenIDRedirectR GET
/wechat_openid_callback WeChatOpenIDCallbackR GET

|]

{-
/lock_pending LockPendingR GET
/refresh_lock_status RefreshLockStatus GET
/responder_respond ResponderRespondR GET
/responce_submit ResponceSubmitR GET
-}

instance Yesod HsacnuLockControl where
  approot = ApprootMaster $ ST.pack . servDomain . appConf

-- Prepared jQuery outsite source
jQueryW :: Widget
jQueryW = addScriptRemote "https://code.jquery.com/jquery-3.2.1.min.js"

-- Handler is the controller of web application (controller in MVC)
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "HsacnuLockControl - Home Page"
  toWidget [hamlet|
    <h1>HsacnuLockControl - Home Page
    <p>
      Welcome to the Hsacnu Lock Control. #
      This system is built by Evrika Logismos Lamda based on Haskell Yesod Web Framework. #
      The owner of the system is Jingze Zhang, the commissioner of the project #
      The purpose of this system is to help you open the lock of valuable properties in the campus. #
    <p>
      Now you're going to use your WeChat OpenID so that we can identify yourself. #
      And after that, you'll receive some options that was reserved in the database. #
      Those options are the representation of specific locks that is active for the moment. #
      Then you're going to choose one of them and leave some optional additional message. #
      Once you've done that, submit your request and the system will send your identity and your message to the proper administrator that possess the premission to grant the lock opening. #
      And when that person received the message on his/her terminal, he/she will make a decision in 5 minutes about whether or not to open the lock, while the mean time, you're going to stay one the pending page and wait for status refresh. #
      If the administrator didn't response in 5 minutes, then your request is outdated and invalidated and you have to restart the procedure for the beginning, which means, here. #
      Also, if you shut the pending webpage accidentally, don't worry, you can still go back to the home page and provide the WeChat OpenID again, then you'll be redirect to the pending page. #
    <p> As far as I can tell, the system is secured and privacy-friendly, and that means I, as the developer of the system, shall not positively collect any of you personel data, including the OpenID itself. They'll be destroyed immediately after the requesting procedure was finished or outdated.
    <p>
      And now, please provide your WeChat OpenID using the following hyper-link.<br>
      <a href=@{WeChatOpenIDRedirectR}>WeChat OpenID Validation
  |]

-- Redirect the user to the WeChat OpenID Login Page
getWeChatOpenIDRedirectR :: Handler ()
getWeChatOpenIDRedirectR = do
  app <- getYesod
  let conf = appConf app
  let encoded = URIE.encode $ callbackDomain conf
  -- FIXME Maybe use getUrlRenderParams instead of this Shakespearean Template for URL generation?
  -- F**k you WeChat, why strong regex matching?
  redirect ([st|
    https://open.weixin.qq.com/connect/oauth2/authorize?appid=#{wcAppId conf}&redirect_uri=#{encoded}&response_type=code&scope=snsapi_userinfo#wechat_redirect
  |])

getWeChatOpenIDCallbackR :: Handler Html
getWeChatOpenIDCallbackR = defaultLayout $ do
  setTitle "HsacnuLockControl - Login Callback"
  wcOpenIdCode <- lookupGetParam "code"
  let Just code = wcOpenIdCode
  app <- getYesod
  let HsacnuLockControlConf {..} = appConf app
  
  let targetAccessToken = [st|https://api.weixin.qq.com/sns/oauth2/access_token?appid=#{wcAppId}&secret=#{wcAppSecret}&code=#{code}&grant_type=authorization_code|]
  requestAccessToken <- parseRequest $ ST.unpack targetAccessToken
  (responseAccessToken :: Response GetAccessTokenResponse) <- httpJSON requestAccessToken
  let accessTokenJSON = getResponseBody responseAccessToken
  let token = accessToken accessTokenJSON
  let userOpenId = openId accessTokenJSON
  
  let targetUserInfo = [st|https://api.weixin.qq.com/sns/userinfo?access_token=#{token}&openid=#{userOpenId}&lang=#{languagePreference}|]
  requestUserInfo <- parseRequest $ ST.unpack targetUserInfo
  -- (userInfoJSON :: ) <- 
  
  toWidget [hamlet|Nothing|]

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

instance YesodPersist HsacnuLockControl where
  type YesodPersistBackend HsacnuLockControl = SqlBackend

  runDB action = do
    HsacnuLockControl {..} <- getYesod
    runSqlPool action connPool

main :: IO ()
main = do
  putStrLn "HsacnuLockControl Project Server-Side Software Version 1.0, initiating..."
  putStrLn "Source Code License: Public Domain License CC0"

  conf <- readJsonFileAndParse
  print conf -- FIXME Debug Only
  pool <- runNoLoggingT $ createSqlitePool "lock.db3" 10
  runSqlPool (runMigration migrateAll) pool
  let appInst = HsacnuLockControl conf pool
  
  putStrLn "Haskell Yesod Warp Web Engine, initiating..."
  warp (servPort conf) appInst
