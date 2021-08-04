{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (
  Api
  )  where

import Data.Text
import Servant
import Data

type Api = DataApi -- :<|> SingerApi

type DataApi = 
  "data" :> "test_table" :> Capture "name" Text :> Get '[JSON] TestTable


