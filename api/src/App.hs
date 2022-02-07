{-# LANGUAGE OverloadedStrings #-}

module App where

import Database.Beam
import Control.Monad.Trans.Except
import Control.Monad
import Data.Text
import Data.String
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding.Base64 as B64
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


data Options = Options {
    optPort        :: Int
  , optDbName      :: String
  , optDbUser      :: String
  , optDbPassword  :: String
  , optS3Url       :: String
  , optS3User      :: String
  , optS3SecretKey :: String 
  , optS3Region    :: String
  }

api :: Proxy Api
api = Proxy

run :: Options -> IO ()
run opts = do
  let port = optPort opts
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp opts

mkApp :: Options -> IO Application
mkApp opts = return $ cors (const $ Just policy) $ provideOptions api $ serve api (server opts)
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ], corsMethods = [methodGet, methodPost, methodDelete, methodOptions] }

dbConnectString :: Options -> BS.ByteString
dbConnectString opts = BS.pack $ "dbname=" ++ optDbName opts ++ " user=" ++ optDbUser opts ++ " password=" ++ optDbPassword opts 

server :: Options -> Server Api
server opts = getMeta opts
  :<|> (getPosts opts :<|> postPosts opts) 
  :<|> (getPostById opts)
  :<|> (uploadImage opts :<|> removeImage)

getPostById :: Options -> Int32 -> Handler B.PostA
getPostById opts id = do
  p' <- liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL $ dbConnectString opts
    runBeamPostgresDebug putStrLn conn (B.getPostById id)
  -- pure p'
  case p' of
    Just p'' -> do
      p''' <- liftIO $ B.postToA p''
      case p''' of
        Left _ -> throwError err404
        Right p -> pure p
    _      -> throwError err404

getPosts :: Options -> Text -> Handler [B.PostA]
getPosts opts tname = do
  ps' <- liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL $ dbConnectString opts
    runBeamPostgresDebug putStrLn conn (B.getPosts tname)
  a <- liftIO $ mapM B.postToA ps'
  let a' :: Either MinioErr [B.PostA]
      a' = sequence a
  case a' of
    Left e -> do
      throwError err404
    Right ps -> do
      pure ps

postPosts :: Options -> Text -> [B.PostA] -> Handler [B.PostA]
postPosts opts tname ts = do
  liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL $ dbConnectString opts
    Pg.withTransaction conn $ runBeamPostgresDebug putStrLn conn $ B.replacePostsWith tname (Prelude.map (B.postFromA tname) ts)
  getPosts opts tname

uploadImage :: Options -> Text -> MultipartData Tmp -> Handler ()
uploadImage opts bucket multipartData = do
  res <- liftIO $ do
    guard $ Prelude.length (files multipartData) > 0
    let file = Prelude.head $ files multipartData
    runMinio s3ConnInfo $ do
      makeBucketIfNotExists bucket
      -- makeBucket bucket (Just "Омск")
      fPutObject bucket (B64.encodeBase64 $ fdFileName file) (fdPayload file) defaultPutObjectOptions
  case res of
    Left err -> do
      liftIO $ putStrLn "----------------"
      liftIO $ putStrLn "MINIO UPLOAD ERROR START"
      liftIO $ putStrLn (show err)
      liftIO $ putStrLn "----------------"
      throwError err404
    Right _ -> pure()

  where creds = Credentials { cAccessKey = pack $ optS3User opts, cSecretKey = pack $ optS3SecretKey opts}
        s3ConnInfo = setCreds creds $ setRegion (pack $ optS3Region opts) (fromString $ optS3Url opts)

removeImage :: Text -> Text -> Handler ()
removeImage bucket name = do
  res <- liftIO $ removeFile bucket name
  case res of
    Left err -> do
      liftIO $ putStrLn (show err)
      throwError err404
    Right _ -> pure()      

getMeta :: Options ->  Handler [B.MetaA]
getMeta opts = 
  liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL $ dbConnectString opts
    runBeamPostgresDebug putStrLn conn (B.getMeta)
