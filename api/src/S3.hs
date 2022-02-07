{-# LANGUAGE OverloadedStrings #-}

module S3 (
  makeBucketIfNotExists  
, getFileUrls  
, removeFile
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



makeBucketIfNotExists :: Region -> Text -> Minio ()
makeBucketIfNotExists region bucket = do
  exists <- bucketExists bucket 
  if not exists then makeBucket bucket (Just region) else pure ()


getFileUrls :: ConnectInfo -> Text -> IO (Either MinioErr [(Text, Text)])
getFileUrls s3ConnInfo bucket = liftIO $ do
    runMinio s3ConnInfo $ do
      exists <- bucketExists bucket
      if exists
        then Conduit.runConduit $ listObjects bucket Nothing True .| ConduitL.mapM getObjectUrl .| ConduitL.catMaybes .| Conduit.sinkList
        else pure []
      -- Conduit.runConduit $ listObjects bucket Nothing True .| ConduitL.mapM getObjectUrl .| ConduitL.catMaybes .| Conduit.decodeUtf8 .| Conduit.sinkList

  where getObjectUrl :: ListItem -> Minio (Maybe (Text, Text))
        getObjectUrl (ListItemObject i) = fmap (\u -> Just (oiObject i, E.decodeUtf8 u)) $ presignedGetObjectUrl bucket (oiObject i) (60*60) [] []
        -- getObjectUrl :: ListItem -> Minio (Maybe ByteString)
        -- getObjectUrl (ListItemObject i) = fmap Just $ presignedGetObjectUrl bucket (oiObject i) (60*60) [] []
        getObjectUrl _ = pure Nothing              

removeFile :: ConnectInfo -> Text -> Text -> IO (Either MinioErr ())
removeFile s3ConnInfo bucket name = do
  liftIO $ do
    runMinio s3ConnInfo $ do
      removeObject bucket name

