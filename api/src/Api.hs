{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (
  Api
  )  where

import Data.Text
import Servant
import Servant.Multipart
import DataTestTable
import DataForHouse
import BaseAvito
import Data.Int

type Api = DataApi -- :<|> SingerApi

type DataApi = "data" :> (TestTableApi :<|> ForHouseApi)

type TestTableApi = "test_table" :>
      (
           Capture "id" Int32 :> Get '[JSON] TestTable
      :<|> Get '[JSON] [TestTable]
      :<|> ReqBody '[JSON] [TestTable] :> Post '[JSON] ()
      )

type ForHouseApi = "for_house" :>
      (
           Capture "id" Int32 :> Get '[JSON] ForHouseA
      :<|> Get '[JSON] [ForHouseA]
      :<|> ReqBody '[JSON] [ForHouseA] :> Post '[JSON] [ForHouseA]
      :<|> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] ()
      )


