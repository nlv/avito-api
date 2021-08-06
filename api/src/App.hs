{-# LANGUAGE OverloadedStrings #-}

module App where

import Database.Beam
import Control.Monad.Trans.Except
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.IO
import Data
import Data.Int
import Api
import qualified Base as B
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as Pg

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
mkApp = return $ simpleCors (serve api server)

server :: Server Api
server =
       getTestTableByName
  :<|> postTestTableRow 

getTestTableByName :: Text -> Handler TestTable
getTestTableByName name = do
  p' <- liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    runBeamPostgresDebug putStrLn conn (B.getTestTableByName name)
  case p' of
    Just p -> pure p
    _      -> throwError err404

postTestTableRow :: Text -> TestTable -> Handler ()
postTestTableRow name t = do
  liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    runBeamPostgresDebug putStrLn conn (B.postTestTable name t)
    pure ()


