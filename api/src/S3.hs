{-# LANGUAGE OverloadedStrings #-}

module S3 (
  makeBucketIfNotExists  
, getFileUrls  
)where

import Control.Monad
import Control.Monad.IO.Class

import Network.Minio
import Data.Text
import Data.Text.Encoding as E
-- import Data.ByteString
import Data.Conduit.List as ConduitL
import Data.Conduit as Conduit
import Data.Conduit.Combinators as Conduit
-- import Control.Monad.Trans.Except



makeBucketIfNotExists :: Text -> Minio ()
makeBucketIfNotExists bucket = do
  exists <- bucketExists bucket 
  if not exists then makeBucket bucket (Just region) else pure ()



getFileUrls :: Text -> IO (Either MinioErr [Text])
getFileUrls bucket = liftIO $ do
    runMinio s3ConnInfo $ do
      exists <- bucketExists bucket
      liftIO $ guard exists
      Conduit.runConduit $ listObjects bucket Nothing True .| ConduitL.mapM getObjectUrl .| ConduitL.catMaybes .| Conduit.sinkList
      -- Conduit.runConduit $ listObjects bucket Nothing True .| ConduitL.mapM getObjectUrl .| ConduitL.catMaybes .| Conduit.decodeUtf8 .| Conduit.sinkList

  where bucket = "forhouse"
        getObjectUrl :: ListItem -> Minio (Maybe Text)
        getObjectUrl (ListItemObject i) = fmap (Just . E.decodeUtf8) $ presignedGetObjectUrl bucket (oiObject i) (60*60) [] []
        -- getObjectUrl :: ListItem -> Minio (Maybe ByteString)
        -- getObjectUrl (ListItemObject i) = fmap Just $ presignedGetObjectUrl bucket (oiObject i) (60*60) [] []
        getObjectUrl _ = pure Nothing              



creds = Credentials { cAccessKey = "minioadmin", cSecretKey = "minioadmin"}

        

region :: Region
region = "Omsk"

s3ConnInfo = setCreds creds $ setRegion "Omsk" "http://localhost:9000"   