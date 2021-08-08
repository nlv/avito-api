{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (
  Api
  )  where

import Data.Text
import Servant
import Data
import Data.Int

type Api = DataApi -- :<|> SingerApi

type DataApi = 
       "data" :> "test_table" :> Capture "id" Int32 :> Get '[JSON] TestTable
  :<|> "data" :> "test_table" :> Get '[JSON] [TestTable]
  :<|> "data" :> "test_table" :> ReqBody '[JSON] [TestTable] :> Post '[JSON] ()


