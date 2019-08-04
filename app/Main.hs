{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Import
import RIO.Process
import System.Environment (getEnv)
import Database.PostgreSQL.Simple
import Data.Pool
import GHC.IO.Exception
import API (runApp)
import Util

main :: IO ()
main = do
  options <-
    return ClientOptions <*> (getEnv "VERBOSE" >>= parseBool (userError "can't parse VERBOSE env var")) <*>
    (getEnv "TABLE_SIZE" >>= parseInt (userError "can't parse TABLE_SIZE env var"))
  connectInfo <-
    return ConnectInfo <*>
      getEnv "PG_HOST" <*>
      (getEnv "PG_PORT" >>= parseInt (userError "can't parse PG_PORT env var") <&> fromInteger . toInteger) <*>
      getEnv "PG_USER" <*>
      getEnv "PG_PASS" <*>
      getEnv "PG_DB_NAME"
  pgPoolSize <- getEnv "PG_POOL_SIZE" >>= parseInt (userError "can't parse PG_POOL_SIZE env var")
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  conn <- createPool (mkConnection connectInfo) close 1 100000 pgPoolSize
  withLogFunc lo $ \lf ->
    let app = App {appLogFunc = lf, appProcessContext = pc, appOptions = options, dbConnectionPool = conn}
     in runRIO app runApp


--mkConnection :: ConnectInfo -> IO Connection
--mkConnection connectionInfo = do
--  connection <- connectPostgreSQL $ postgreSQLConnectionString connectionInfo
--  res <- select1 connection
--  print res
--  return connection
--
--select1 :: Connection -> IO Int
--select1 conn = do
-- [Only res] <- query_ conn "select 1"
-- return res
--
----connectionInfo :: ConnectInfo
----connectionInfo = ConnectInfo {
----  connectHost = "localhost",
----  connectPort = 5432,
----  connectUser = "postgres",
----  connectPassword = "",
----  connectDatabase = "postgres"
----}
--
--parseBool :: MonadError e m => e -> String -> m Bool
--parseBool _ "true" = return True
--parseBool _ "false" = return False
--parseBool e _ = throwError e
--
--parseInt :: MonadError e m => e -> String -> m Int
--parseInt t n =
--  case readMaybe n :: Maybe Int of
--    Just i -> return i
--    Nothing -> throwError t
