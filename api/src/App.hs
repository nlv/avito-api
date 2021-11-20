{-# LANGUAGE OverloadedStrings #-}

module App where

import Database.Beam
import Control.Monad.Trans.Except
import Control.Monad
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Servant
import Servant.Multipart
import System.IO
import DataTestTable
import DataForHouse
import Data.Int
import Api
import qualified BaseAvito as B
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as Pg

import Network.Minio
import S3

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
       ((getTestTableById
  :<|> getTestTable     
  :<|> postTestTable) :<|> 
       (getForHouseById
  :<|> getForHouse     
  :<|> postForHouse
       ))
  :<|> uploadFile

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

getForHouseById :: Int32 -> Handler B.ForHouseA
getForHouseById id = do
  p' <- liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    runBeamPostgresDebug putStrLn conn (B.getForHouseById id)
  -- pure p'
  case p' of
    Just p'' -> do
      p''' <- liftIO $ B.forHouseToA p''
      case p''' of
        Left _ -> throwError err404
        Right p -> pure p
    _      -> throwError err404

getForHouse :: Handler [B.ForHouseA]
getForHouse = do
  ps' <- liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    runBeamPostgresDebug putStrLn conn (B.getForHouse)
    -- runBeamPostgresDebug putStrLn conn (runSelectReturningList $ select $ all_ (B.avitoDb ^. B.testTable))
  a <- liftIO $ mapM B.forHouseToA ps'
  let a' :: Either MinioErr [B.ForHouseA]
      a' = sequence a
  case a' of
    Left e -> do
      liftIO $ putStrLn "222222 "
      liftIO $ putStrLn $ show e
      throwError err404
    Right ps -> do
      liftIO $ putStrLn "111111 "
      liftIO $ mapM_  (putStrLn . show) ps
      pure ps

postForHouse :: [B.ForHouseA] -> Handler [B.ForHouseA]
postForHouse ts = do
  liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    Pg.withTransaction conn $ runBeamPostgresDebug putStrLn conn $ B.replaceForHouseWith (Prelude.map B.forHouseFromA ts)
  getForHouse  

uploadFile :: Text -> MultipartData Tmp -> Handler ()
uploadFile bucket multipartData = do
  res <- liftIO $ do
    guard $ Prelude.length (files multipartData) > 0
    let file = Prelude.head $ files multipartData
    runMinio s3ConnInfo $ do
      makeBucketIfNotExists bucket
      -- makeBucket bucket (Just "Омск")
      fPutObject bucket (fdFileName file) (fdPayload file) defaultPutObjectOptions
  case res of
    Left err -> do
      liftIO $ putStrLn (show err)
      throwError err404
    Right _ -> pure()

  where creds = Credentials { cAccessKey = "minioadmin", cSecretKey = "minioadmin"}
        s3ConnInfo = setCreds creds $ setRegion "Omsk" "http://localhost:9000" 

