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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import System.Environment (getArgs)
import Control.Exception hiding (Handler)

import Yesod.Core
import Text.Hamlet
import Text.Blaze.Html.Renderer.String

import qualified Network.HTTP.Types as H

-- Application routing/database/resource configuration, no modification unless
-- you are extremely certain what they're and what are their function.
data HsacnuLockControl =
  HsacnuLockControlWithConfig {
    wcOpenId :: String,
    dbUrl :: String,
    dbUser :: String,
    dbPswd :: String
  } | HsacnuLockControl
-- $(makeLenses ''HsacnuLockControlWithConfig)

$(mkYesod "HsacnuLockControl" [parseRoutes|
/ HomeR GET
/wechat_openid_redirect WeChatOpenIDRedirectR GET
/wechat_openid_callback WeChatOpenIDCallback GET
|])

instance Yesod HsacnuLockControl

-- UserInfo Enum Sex
data Sex = Male | Female | Unspecified deriving (Eq, Enum, Show, Read)

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
  } | ResponserUser {
    placeholder :: () -- TODO Repsonser
  } deriving (Eq, Show, Read)

jQueryW :: Widget
jQueryW = addScriptRemote "https://code.jquery.com/jquery-3.2.1.min.js"

-- Home page Widget (welcome info and continue button)
homePageP :: Widget
homePageP = do
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

-- Handler is the controller of web application (controller in MVC)
getHomeR :: Handler Html
getHomeR = defaultLayout homePageP

-- Redirect the user to the WeChat OpenID Login Page
getWeChatOpenIDRedirectR :: Handler ()
getWeChatOpenIDRedirectR = redirect ("https://google.com" :: String)

-- Default web-server port, you may modify it if you want to
defaultPort :: Int
defaultPort = 3000

-- This is the function that used to extract the information about port that we
-- are using from the command line arguments, to be more specific, the first.
-- And if there's no argument or the format of the first one is not number, the
-- port number stored in `defaultPort` will will used.
httpPort :: IO Int
httpPort = do
  args <- getArgs
  let port = ((read (head args)) :: Int)
  if null args
    then do
      putStrLn "Port option missing, using default value 3000."
      return defaultPort
    else handle (\(SomeException _) ->
        putStrLn "Port option error, using default value 3000" >> return defaultPort)
        (putStrLn ("Port option " ++ (show port) ++ " received.") >> return port)

-- Entry function of the application.
-- Print something irrelevant and boring >>
-- Load the port number >>
-- Load some settings files >>
-- Initiate the Warp Engine
main :: IO ()
main = do
  putStrLn "HsacnuLockControl Project Server-Side Software Version 1.0, initiating......"
  putStrLn "Author: Evrika Logismos Lamda"
  putStrLn "Release Date: Stardate -94822.0"
  putStrLn "Source Code License: GNU_GPL License Version 3.0"
  port <- httpPort
  putStrLn "Haskell Yesod Warp Web Engine, initiating......"
  warp port HsacnuLockControl

