{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE FlexibleInstances #-}
module Types where

import RIO
import RIO.Process
import Data.Pool

import Data.Aeson

import Database.PostgreSQL.Simple

import Servant.API

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

type API = GetIdeaRoute :<|> SaveIdeaRoute

data Idea = Idea { ideaId :: Int
                 , text :: String } deriving (Eq, Show, Generic, FromRow)

instance ToJSON Idea

newtype IdeaForm = IdeaForm { body :: String } deriving (Eq, Show, Generic)

instance FromJSON IdeaForm
