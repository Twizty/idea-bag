{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE FlexibleInstances #-}

module API (runApp) where

import Data.Functor

import Data.Pool (withResource)

import Database.PostgreSQL.Simple

import Servant.API

import Control.Monad.Except
import Control.Monad.Reader

import Network.Wai
import Network.Wai.Handler.Warp

import Types
import RIO (RIO, runRIO)

import Servant

class MonadIO m => IdeaMonad m where
  get :: m (Maybe Idea)
  save :: String -> m ()

instance IdeaMonad (RIO App) where
  get = getIdea
  save text = do
    App _ _ ClientOptions { tableSize } pool <- ask
    liftIO $ withResource pool $ \conn -> withTransaction conn $ saveIdea tableSize text conn


getIdea :: RIO App (Maybe Idea)
getIdea = do
  App _ _ ClientOptions { tableSize } pool <- ask
  liftIO $ withResource pool $ getRandomIdea tableSize

checkTableSize :: Connection -> Int -> IO ()
checkTableSize conn size = do
  [Only currSize] <- query_ conn "select count(*) from ideas"
  when (currSize >= size) $ throwError $ userError "Too many ideas in the database"

getRandomIdea :: Int -> Connection -> IO (Maybe Idea)
getRandomIdea size conn = withTransaction conn $ do
  checkTableSize conn size
  ideas <- query_ conn "select * from ideas offset (select floor(random() * count(*) + 1)::int from ideas) - 1 limit 1"
  case ideas of
    idea : _ -> execute conn "delete from ideas where id = ?" [ideaId idea] $> Just idea
    _ -> return Nothing

saveIdea :: Int -> String -> Connection -> IO ()
saveIdea size text conn = do
  checkTableSize conn size
  void $ execute conn "insert into ideas (text) values (?)" [text]

getOrElse :: MonadError e m => m a -> Maybe a -> m a
getOrElse _ (Just a) = return a
getOrElse m Nothing = m

handlerGet :: App -> Handler Idea
handlerGet env = liftIO (runRIO env get) >>= getOrElse (throwError err404)

handlerPost :: App -> IdeaForm -> Handler String
handlerPost env IdeaForm { body } = liftIO $ runRIO env $ save body $> ""

serverGet :: App -> Server GetIdeaRoute
serverGet = handlerGet

serverPost :: App -> Server SaveIdeaRoute
serverPost = handlerPost

proxy :: Proxy API
proxy = Proxy

runApi :: App -> Application
runApi env = serve proxy $ serverGet env :<|> serverPost env

runApp :: RIO App ()
runApp = do
  env <- ask
  liftIO $ run 8081 (runApi env)

