{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-#  LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module BaseAvito (
    AvitoDb(..)
  , avitoDb

  , Post(..)
  , PostA(..)
  , getPostById
  , getPosts
  , replacePostsWith  
  , postToA
  , postFromA
  )  where

import GHC.Generics
import Database.Beam
import Database.Beam.Backend.SQL
import qualified Database.Beam.Postgres as Pg
import Network.Minio
import Data.List as List
import Data.Int
import Data.Text
import Data.Aeson
import Lens.Micro
import Post

import S3

data AvitoDb f = AvitoDb
                      { _posts :: f (TableEntity PostT) 
                      }
                        deriving Generic

instance Database be AvitoDb

avitoDb :: DatabaseSettings be AvitoDb
avitoDb = defaultDbSettings `withDbModification`
  dbModification {
     _posts = setEntityName "posts" <>
                    modifyTableFields tableModification {
                       _postIdT           = fieldNamed "id"
                    ,  _postTnameT        = fieldNamed "tname"
                    ,  _postOidT          = fieldNamed "oid"
                    ,  _postCategoryT     = fieldNamed "category"
                    ,  _postTitleT        = fieldNamed "title"
                    ,  _postDescriptionT  = fieldNamed "description"
                    ,  _postPriceT        = fieldNamed "price"
                    ,  _postVideoUrlT     = fieldNamed "video_url"
                    ,  _postAddrRegionT   = fieldNamed "addr_region"
                      -- _postAddrAreaT  = fieldNamed "addr_area",
                    ,  _postAddrCityT     = fieldNamed "addr_city"
                    ,  _postAddrPointT    = fieldNamed "addr_point"
                    ,  _postAddrStreetT   = fieldNamed "addr_street"
                    ,  _postAddrHouseT    = fieldNamed "addr_house"
                    ,  _postContactPhoneT = fieldNamed "contact_phone"
                    ,  _postPostT         = fieldNamed "post"
                    }
  }

AvitoDb 
  (TableLens posts) = dbLenses   

instance Table PostT where
  data PrimaryKey PostT f = PostId (Columnar f Int32) deriving Generic
  primaryKey = PostId . _postIdT 

type PostId = PrimaryKey PostT Identity

instance Beamable (PrimaryKey PostT)

instance Beamable PostT

Post
  (LensFor postId)    
  (LensFor postTname)
  (LensFor postOid)
  (LensFor postCategory)
  (LensFor postTitle)
  (LensFor postDescription)
  (LensFor postPrice)
  (LensFor postVideoUrl)
  (LensFor postAddrRegion)
  -- (LensFor postAddrArea)
  (LensFor postAddrCity)
  (LensFor postAddrPoint)
  (LensFor postAddrStreet)
  (LensFor postAddrHouse)
  (LensFor postContactPhone) 
  (LensFor postPost)
  = tableLenses  

getPostById id = do
  ps <- runSelectReturningList $ lookup_ (avitoDb ^. posts) (PostId id)
  pure $ if List.length ps > 0 then Just $ List.head ps else Nothing

getPosts :: Text -> Pg.Pg [Post]
getPosts tname = do
  ps <- runSelectReturningList $ select $ do
    p <- all_ (avitoDb ^. posts)  
    guard_ (p ^. postTname ==. val_ tname)
    pure p
  pure ps  

-- TODO ts должен содержать tname
replacePostsWith tname ts = do
  runDelete $ Database.Beam.delete (avitoDb ^. posts) (\r -> r ^. postTname ==. val_ tname)
  runInsert $ Database.Beam.insert (avitoDb ^. posts) (insertValues ts)

data PostA
  = PostA {
      _postId           :: Int32
    , _postOid          :: Text
    , _postCategory     :: Maybe Text
    , _postTitle        :: Maybe Text
    , _postDescription  :: Maybe Text
    , _postPrice        :: Maybe Text
    , _postImageUrl     :: [(Text, Text)]
    , _postVideoUrl     :: Maybe Text
    , _postAddrRegion   :: Maybe Text
    -- _postAddrArea   :: Columnar f (Maybe Text)
    , _postAddrCity     :: Maybe Text
    , _postAddrPoint    :: Maybe Text
    , _postAddrStreet   :: Maybe Text
    , _postAddrHouse    :: Maybe Text
    , _postContactPhone :: Maybe Text
    , _postPost         :: Value
  } 
  deriving (Generic, Show)

instance ToJSON (PostA)
instance FromJSON (PostA)

postToA :: PostT Identity -> IO (Either MinioErr PostA)
postToA h = do 
  urls <- getFileUrls (h ^. postOid) 
  pure $ fmap (f h) urls
  where f :: PostT Identity -> [(Text, Text)] -> PostA
        f h1 urls = 
          PostA {
            _postId           = h1 ^. postId
          , _postOid          = h1 ^. postOid
          , _postCategory     = h1 ^. postCategory
          , _postTitle        = h1 ^. postTitle
          , _postDescription  = h1 ^. postDescription
          , _postPrice        = h1 ^. postPrice
          , _postImageUrl     = urls
          , _postVideoUrl     = h1 ^. postVideoUrl
          , _postAddrRegion   = h1 ^. postAddrRegion
           -- _postAddrArea  f (Maybe Text),
          , _postAddrCity     = h1 ^. postAddrCity
          , _postAddrPoint    = h1 ^. postAddrPoint
          , _postAddrStreet   = h1 ^. postAddrStreet
          , _postAddrHouse    = h1 ^. postAddrHouse
          , _postContactPhone = h1 ^. postContactPhone
          , _postPost         = post
          }
          where Pg.PgJSONB post =  (h1 ^. postPost) 

postFromA :: Text -> PostA -> Post
postFromA tname a = Post {
      _postIdT           = _postId a
    , _postTnameT        = tname
    , _postOidT          = _postOid a
    , _postCategoryT     = _postCategory  a
    , _postTitleT        = _postTitle a
    , _postDescriptionT  = _postDescription a
    , _postPriceT        = _postPrice a
    , _postVideoUrlT     = _postVideoUrl a
    , _postAddrRegionT   = _postAddrRegion a
    , _postAddrCityT     = _postAddrCity a
    , _postAddrPointT    = _postAddrPoint a
    , _postAddrStreetT   = _postAddrStreet a
    , _postAddrHouseT    = _postAddrHouse a
    , _postContactPhoneT = _postContactPhone a
    , _postPostT         = Pg.PgJSONB $ _postPost a
}
