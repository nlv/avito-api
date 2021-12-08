{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Post (
  Post,
  PostT(..),
  PostId
  )  where

import GHC.Generics
import Data.Aeson
import Data.Text
import Data.Int
import Database.Beam
import Database.Beam.Postgres

data PostT f
  = Post {
    _postIdT           :: Columnar f Int32,
    _postTnameT        :: Columnar f Text,
    _postOidT          :: Columnar f Text,
    _postCategoryT     :: Columnar f (Maybe Text),
    _postTitleT        :: Columnar f (Maybe Text),
    _postDescriptionT  :: Columnar f (Maybe Text),
    _postPriceT        :: Columnar f (Maybe Text),
    _postVideoUrlT     :: Columnar f (Maybe Text),
    _postAddrRegionT   :: Columnar f (Maybe Text),
    -- _postAddrAreaT  :: Columnar f (Maybe Text),
    _postAddrCityT     :: Columnar f (Maybe Text),
    _postAddrPointT    :: Columnar f (Maybe Text),
    _postAddrStreetT   :: Columnar f (Maybe Text),
    _postAddrHouseT    :: Columnar f (Maybe Text),
    _postContactPhoneT :: Columnar f (Maybe Text),
    _postPostT         :: Columnar f (PgJSONB Value)
  }
  deriving Generic



type Post = PostT Identity

deriving instance Show Post
deriving instance Eq Post

-- instance Generic (PgJSONB Data.Aeson.Value)

instance ToJSON (PgJSONB Data.Aeson.Value) where
  toJSON (PgJSONB a) = toJSON a
  toEncoding (PgJSONB a) = toEncoding a

instance FromJSON (PgJSONB Data.Aeson.Value) where 
  parseJSON = pure . PgJSONB

instance ToJSON (PostT Identity)
instance FromJSON (PostT Identity)

type PostId = PrimaryKey PostT Identity



