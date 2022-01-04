{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (
  Api
  )  where

import Data.Text
import Servant
import Servant.Multipart
import Post
import BaseAvito
import Data.Int

type Api = MetaApi :<|> DataApi :<|> RowApi :<|> ImageApi

type MetaApi = 
     "meta" :> (
          Get '[JSON] [MetaA]
     )

type DataApi = 
     "data" :> (
          Capture "tname" Text :> Get '[JSON] [PostA] 
     :<|> Capture "tname" Text :> ReqBody '[JSON] [PostA] :> Servant.Post '[JSON] [PostA]
     )

type RowApi = "row" :> 
      (
           Capture "oid" Int32 :> Get '[JSON] PostA
      )      

type ImageApi = "images" :> 
     (
          Capture "id" Text :> MultipartForm Tmp (MultipartData Tmp) :> Servant.Post '[JSON] ()
     :<|> Capture "id" Text :> Capture "image" Text :> Delete '[JSON] ()
     )      


