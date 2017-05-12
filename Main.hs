{-
-- Project Hsacnu Lock Control
-- Author: Yutong Zhang
-}

module Main where

import qualified Network.URI.Encode as URIE
import qualified Data.Text as ST
import qualified Data.ByteString.Lazy as BS

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Text.Read (readMaybe)
import Data.Aeson (decode, encode, eitherDecode, FromJSON, ToJSON, (.:))

import Yesod.Core
import Text.Hamlet
import Text.Shakespeare.Text
import Text.Blaze.Html.Renderer.String
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

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
  approot = ApprootMaster $ ST.pack . servDomain

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
  HsacnuLockControl {..} <- getYesod
  urlRender <- getUrlRender
  let encoded = URIE.encode $ ST.unpack $ urlRender WeChatOpenIDCallbackR
  -- FIXME Maybe use getUrlRenderParams instead of this Shakespearean Template for URL generation?
  -- F**k you WeChat, why strong regex matching?
  redirect ([st|
    https://open.weixin.qq.com/connect/oauth2/authorize?appid=#{wcAppId}&redirect_uri=#{encoded}&response_type=code&scope=snsapi_userinfo#wechat_redirect
  |])

getWeChatOpenIDCallbackR :: Handler Html
getWeChatOpenIDCallbackR = defaultLayout $ do
  wcOpenIDCode <- lookupGetParam "code"
  wcCallbackState <- lookupGetParam "state"
  setTitle "HsacnuLockControl - Login Callback"
  if wcOpenIDCode == Nothing then
    -- Error message placeholder
    toWidget [hamlet|
      <h1>Authorization failed!
      <p>
        The cause of this failure is WeChat Open ID Platform, and both of the service provider and the developer has nothing to do with it.
      <p>
        Following contents are debug-only, thus it may contain the critical personal information about you and your WeChat account.
        For this reason, we suggest you to ignore this message and try again later if you are not one of our technician.
        You may also send a email to our developer, but be reminded that you should wipe out your personal information first.
        <font color="red">You've been warned.
      <p>
        The url requesting this page should have the scheme:
        <br>
        redirect_uri?code=CODE&state=STATE
        <br>
        But clearly the system didn't receive the get parameter named "code".
        The full redirect uri is:
        <br>
        @{WeChatOpenIDCallbackR}
      <p>
        We apologize for this interruption, please proceed.
        Yutong Zhang
    |]
  else
    toWidget [hamlet|
      <h1>Incomplete functionality!
      <p>TODO Gather information from the API and the snsapi_code, and then write to the persistent storage.
      <p>
        Debug info:
        <br>
        snsapi_callback_code = #
        $maybe code <- wcOpenIDCode
          #{code}
        $nothing
          Nothing
        <br>
        snsapi_callback_state = #
        $maybe state <- wcCallbackState
          #{state}
        $nothing
          Nothing
    |]

parseJsonConf :: BS.ByteString -> HsacnuLockControl
parseJsonConf src = case eitherDecode src of
  Right d -> d
  Left e -> error $ "ERROR: Failure during parsing the JSON config file, details in: \n" ++ e

jsonConfFileName :: FilePath
jsonConfFileName = "config.json"

readJsonFileAndParse :: IO HsacnuLockControl
readJsonFileAndParse = do
  fileContent <- BS.readFile jsonConfFileName
  return $ parseJsonConf fileContent

main :: IO ()
main = do
  putStrLn "HsacnuLockControl Project Server-Side Software Version 1.0, initiating..."
  putStrLn "Author: Evrika Logismos Lamda"
  putStrLn "Release Date: Stardate -94822.0"
  putStrLn "Source Code License: Public Domain License CC0"

  appInst <- readJsonFileAndParse

  print appInst -- FIXME Debug Only
  
  putStrLn "Haskell Yesod Warp Web Engine, initiating..."
  warp (servPort appInst) appInst
