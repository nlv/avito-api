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
import Network.HTTP.Types.Method
import Servant
import Servant.Multipart
import System.IO
import Post
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
    policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ], corsMethods = [methodGet, methodPost, methodDelete, methodOptions] }

server :: Server Api
server = getMeta
  :<|> (getPosts :<|> postPosts) 
  :<|> (getPostById)
  :<|> (uploadImage :<|> removeImage)

getPostById :: Int32 -> Handler B.PostA
getPostById id = do
  p' <- liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    runBeamPostgresDebug putStrLn conn (B.getPostById id)
  -- pure p'
  case p' of
    Just p'' -> do
      p''' <- liftIO $ B.postToA p''
      case p''' of
        Left _ -> throwError err404
        Right p -> pure p
    _      -> throwError err404

getPosts :: Text -> Handler [B.PostA]
getPosts tname = do
  ps' <- liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    runBeamPostgresDebug putStrLn conn (B.getPosts tname)
  a <- liftIO $ mapM B.postToA ps'
  let a' :: Either MinioErr [B.PostA]
      a' = sequence a
  case a' of
    Left e -> do
      throwError err404
    Right ps -> do
      pure ps

postPosts :: Text -> [B.PostA] -> Handler [B.PostA]
postPosts tname ts = do
  liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    Pg.withTransaction conn $ runBeamPostgresDebug putStrLn conn $ B.replacePostsWith tname (Prelude.map (B.postFromA tname) ts)
  getPosts tname

uploadImage :: Text -> MultipartData Tmp -> Handler ()
uploadImage bucket multipartData = do
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

removeImage :: Text -> Text -> Handler ()
removeImage bucket name = do
  res <- liftIO $ removeFile bucket name
  case res of
    Left err -> do
      liftIO $ putStrLn (show err)
      throwError err404
    Right _ -> pure()      

getMeta :: Handler [B.MetaA]
getMeta = 
  liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=avito user=nlv password=1" 
    runBeamPostgresDebug putStrLn conn (B.getMeta)
