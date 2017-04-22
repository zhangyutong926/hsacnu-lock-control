{-
-- Greetings, Jingze Zhang.
-- This is a message from the author Yutong Zhang (Evrika Logismos Lamda).
-- I would like to inform you about the code and its usage.
-- Please read my code carefully and find out the corrent position to plug-in your configuration data.
-- Just for your information, whatever platform you're using, you'll need to install a tool called HaskellStack to build the program.
--
-- You may already noticed, unlike other web server-side softwares always running in a container like Tomcat or Jetty, this program is independently compiled and will be run directly on the native OS environment.
-- And this feature is important for the performance of the server-side program.
-- The program has to be built in every OS platform you want it to run on.
--
-- Here's the procedure you may require to in order to build the correct distribution binary.
-- 1. Install the Haskell Stack Build Tool from https://docs.haskellstack.org/en/stable/README/
-- 2. Navigate inside the project directory
-- 3. Use command stack build
-- 4. Find the location of target binary from the Stack's build log, it will be like:
--    "/home/zhangyutong926/Workspace/YesodDemo/HsacnuLockControl/.stack-work/install/x86_64-linux/lts-8.5/8.0.2/bin"
--    And it directly follows a log:
--    "Installing executable(s) in"
-- 5. Use your shell command to run it.
-- Here's another special notification: you will terminate neither the process nor the terminal you were using to run the program, because by doing that, you will cause a service termination.
--
-- All question can be sent to my email, which is yotochang@gmail.com, but spam is not welcomed and will be blacklisted.
--
-- Yoto Chang (Evrika Logismos Lamda)
-- Mar. 22nd, 2017 C.E.
-}

module Main where

import qualified Network.URI.Encode as URIE
import qualified Data.Text as ST

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Text.Read (readMaybe)

import Yesod.Core
import Text.Hamlet
import Text.Shakespeare.Text
import Text.Blaze.Html.Renderer.String
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import SexEnum

-- Application routing/database/resource configuration, no modification unless
-- you are extremely certain what they're and what are their function.
data HsacnuLockControl =
  HsacnuLockControl {
    servPort :: Int,
    wcAppId :: String,
    dbUrl :: String,
    dbUser :: String,
    dbPswd :: String
  } | HsacnuLockControlConfFail
    deriving (Eq, Show, Read) -- | HsacnuLockControl
-- $(makeLenses ''HsacnuLockControlWithConfig)

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

mkYesod "HsacnuLockControl" [parseRoutes|
/ HomeR GET
/wechat_openid_redirect WeChatOpenIDRedirectR GET
/wechat_openid_callback WeChatOpenIDCallbackR GET
|]

instance Yesod HsacnuLockControl where
  approot = ApprootStatic "http://localhost:3000" -- For debug only

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

-- Default web-server port, you may modify it if you want to
defaultPort :: Int
defaultPort = 3000

-- Use this function to extract port, wcAppId, db url, db username, db password from
-- command line interface arguments.
-- The function may fail so you may want to use a try or a handle
-- TODO Rewrite this with YAML config file!
cliArgConf :: [String] -> Maybe HsacnuLockControl
cliArgConf [portRaw, wcAppId, dbUrl, dbUser, dbPswd] =
  port >>= (\port -> Just HsacnuLockControl {
    servPort = port,
    wcAppId = wcAppId,
    dbUrl = dbUrl,
    dbUser = dbUser,
    dbPswd = dbPswd
  })
  where
    port :: Maybe Int
    port = readMaybe portRaw
cliArgConf _ = Nothing

-- Entry function of the application.
-- Print something irrelevant and boring >>
-- Load the port number >>
-- Load some settings files >>
-- Initiate the Warp Engine
main :: IO ()
main = do
  putStrLn "HsacnuLockControl Project Server-Side Software Version 1.0, initiating..."
  putStrLn "Author: Evrika Logismos Lamda"
  putStrLn "Release Date: Stardate -94822.0"
  putStrLn "Source Code License: GNU_GPL License Version 3.0"

  args <- getArgs
  let parsedConf = (cliArgConf args)
  appInst <- if parsedConf == Nothing
    then do
      putStrLn "Incorrect argument number or type, server terminating..."
      exitWith $ ExitFailure 1
      return undefined
    else
      return ((\(Just a) -> a) parsedConf)

  putStrLn "Haskell Yesod Warp Web Engine, initiating..."
  warp (servPort appInst) appInst
