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

postTestTableRow :: Text -> TestTableR -> Handler ()
postTestTableRow name tr = do
  liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    runBeamPostgresDebug putStrLn conn (B.postTestTable name tr)
    pure ()


