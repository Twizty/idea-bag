{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE FlexibleInstances #-}
module Types where

import RIO
import RIO.Process
import Data.Pool

import Data.Aeson

import Database.PostgreSQL.Simple

import Servant.API

import qualified Network.HTTP.Media               as M
import qualified Data.ByteString.Lazy.Char8       as BC

-- | Command line arguments
data ClientOptions = ClientOptions
  { optionsVerbose :: !Bool
  , tableSize :: !Int
  } deriving Show

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !ClientOptions
  , dbConnectionPool :: !(Pool Connection)
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

type GetIdeaRoute = "api" :> "idea" :> Get '[JSON] Idea
type SaveIdeaRoute = "api" :> "idea" :> ReqBody '[JSON] IdeaForm :> Post '[PlainText] String
type IndexRoute = Get '[HTML] String
type StyleRoute = "style.css" :> Get '[TextCSS] String
type FastOptLibRoute = "frontend-fastopt-library.js" :> Get '[ApplicationJavaScript] String
type FastOptLoadRoute = "frontend-fastopt-loader.js" :> Get '[ApplicationJavaScript] String
type FastOptRoute = "frontend-fastopt.js" :> Get '[ApplicationJavaScript] String
type AppRoute = CaptureAll "other" Text :> Get '[HTML] String

data TextCSS

instance Accept TextCSS where
  contentType _ = "text" M.// "css" M./: ("charset", "utf-8")

instance MimeRender TextCSS String where
  mimeRender _ = BC.pack

data ApplicationJavaScript

instance Accept ApplicationJavaScript where
    contentType _ = "application" M.// "javascript" M./: ("charset", "utf-8")

instance MimeRender ApplicationJavaScript String where
  mimeRender _ = BC.pack

data HTML

instance Accept HTML where
  contentType _ = "text" M.// "html" M./: ("charset", "utf-8")

instance MimeRender HTML String where
  mimeRender _ = BC.pack

type API = GetIdeaRoute :<|>
           SaveIdeaRoute :<|>
           IndexRoute :<|>
           StyleRoute :<|>
           FastOptLibRoute :<|>
           FastOptLoadRoute :<|>
           FastOptRoute :<|>
           AppRoute

data Idea = Idea { ideaId :: Int
                 , text :: String } deriving (Eq, Show, Generic, FromRow)

instance ToJSON Idea

newtype IdeaForm = IdeaForm { body :: String } deriving (Eq, Show, Generic)

instance FromJSON IdeaForm
