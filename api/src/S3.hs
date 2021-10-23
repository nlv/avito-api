{-# LANGUAGE OverloadedStrings #-}

module S3 (
  makeBucketIfNotExists  
)where

import Network.Minio
import Data.Text
-- import Control.Monad.Trans.Except



makeBucketIfNotExists :: Text -> Minio ()
makeBucketIfNotExists bucket = do
  exists <- bucketExists bucket 
  if not exists then makeBucket bucket (Just region) else pure ()

region :: Region
region = "Omsk"