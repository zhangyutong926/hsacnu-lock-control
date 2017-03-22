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

module Main where

import System.Environment (getArgs)
import Control.Exception hiding (Handler)

import Yesod.Core
import Text.Hamlet
import Text.Blaze.Html.Renderer.String

-- Application routing/database/resource configuration, no modification unless
-- you are extremely certain what they're and what are their function.
data HsacnuLockControl = HsacnuLockControl

$(mkYesod "HsacnuLockControl" [parseRoutes|
/ HomeR GET
|])

instance Yesod HsacnuLockControl

htmlTest :: Html
htmlTest = [shamlet|
<p>Greetings.
<p>System functioning normally.
<p>Please designate your intention.
|]

-- UserInfo Enum Sex
data Sex = Male | Female | Unspecified deriving (Eq, Enum, Show, Read)

-- UserInfo Primary ADT
data UserInfo = RequesterUser {
                                openId :: String,
                                nickName :: String,
                                sex :: Sex,
                                province :: String,
                                city :: String,
                                country :: String,
                                headImageUrl :: String,
                                privilege :: [String],
                                unionId :: String
                              }
              | ResponserUser {
                                placeholder :: ()
                              } deriving (Eq, Show, Read)

-- Handler is the controller of web application (controller in MVC)
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

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

