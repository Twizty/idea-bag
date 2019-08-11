{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( mkConnection
  , parseBool
  , parseInt
  , createTable
  ) where

import RIO
import Database.PostgreSQL.Simple
import Control.Monad.Except

mkConnection :: ConnectInfo -> IO Connection
mkConnection connectionInfo = do
  connection <- connectPostgreSQL $ postgreSQLConnectionString connectionInfo
  _ <- select1 connection
  return connection

select1 :: Connection -> IO Int
select1 conn = do
  [Only res] <- query_ conn "select 1"
  return res

createTable :: Connection -> IO ()
createTable conn = void $ execute conn "create table if not exists ideas (id bigserial primary key, text text);" ()

--connectionInfo :: ConnectInfo
--connectionInfo = ConnectInfo {
--  connectHost = "localhost",
--  connectPort = 5432,
--  connectUser = "postgres",
--  connectPassword = "",
--  connectDatabase = "postgres"
--}

parseBool :: MonadError e m => e -> String -> m Bool
parseBool _ "true" = return True
parseBool _ "false" = return False
parseBool e _ = throwError e

parseInt :: MonadError e m => e -> String -> m Int
parseInt t n =
  case readMaybe n :: Maybe Int of
    Just i -> return i
    Nothing -> throwError t
