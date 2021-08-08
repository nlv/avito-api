{-# LANGUAGE OverloadedStrings #-}

module App where

import Database.Beam
import Control.Monad.Trans.Except
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Servant
import System.IO
import Data
import Data.Int
import Api
import qualified Base as B
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as Pg

-- import Lens.Micro

api :: Proxy Api
api = Proxy

run :: IO ()
run = do
  let port = 3030
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ cors (const $ Just policy) $ provideOptions api $ serve api server
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] }

server :: Server Api
server =
       getTestTableById
  :<|> getTestTable     
  :<|> postTestTable

getTestTableById :: Int32 -> Handler TestTable
getTestTableById id = do
  p' <- liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    runBeamPostgresDebug putStrLn conn (B.getTestTableById id)
  -- pure p'
  case p' of
    Just p -> pure p
    _      -> throwError err404

getTestTable :: Handler [TestTable]
getTestTable = do
  ps <- liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    runBeamPostgresDebug putStrLn conn (B.getTestTable)
    -- runBeamPostgresDebug putStrLn conn (runSelectReturningList $ select $ all_ (B.avitoDb ^. B.testTable))
  pure ps

postTestTable :: [TestTable] -> Handler ()
postTestTable ts = do
  liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    Pg.withTransaction conn $ runBeamPostgresDebug putStrLn conn $ B.replaceTestTableWith ts
    pure ()


