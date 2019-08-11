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
  pool <- createPool (mkConnection connectInfo) close 1 100000 pgPoolSize
  withResource pool createTable
  withLogFunc lo $ \lf ->
    let app = App {appLogFunc = lf, appProcessContext = pc, appOptions = options, dbConnectionPool = pool}
     in runRIO app runApp
