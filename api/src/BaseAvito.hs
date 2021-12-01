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
  , testTable

  , getTestTableById
  , getTestTable
  , replaceTestTableWith  

  , ForHouse(..)
  , ForHouseA(..)
  , getForHouseById
  , getForHouse
  , replaceForHouseWith  
  , forHouseToA
  , forHouseFromA
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
import DataTestTable
import DataForHouse

import S3

data AvitoDb f = AvitoDb
                      { _testTable :: f (TableEntity TestTableT) 
                      , _forHouse  :: f (TableEntity ForHouseT) 
                      }
                        deriving Generic

instance Database be AvitoDb

avitoDb :: DatabaseSettings be AvitoDb
avitoDb = defaultDbSettings `withDbModification`
  dbModification {
      _testTable = setEntityName "test_table" <>
                    modifyTableFields tableModification {
                      _testTableId   = fieldNamed "id",
                      _testTableCol1 = fieldNamed "col1",
                      _testTableCol2 = fieldNamed "col2",
                      _testTableCol3 = fieldNamed "col3"
                    }
    ,_forHouse = setEntityName "for_house" <>
                    modifyTableFields tableModification {
                      _forHouseIdT           = fieldNamed "id",
                      _forHouseOidT          = fieldNamed "oid",
                      _forHouseCategoryT     = fieldNamed "category",
                      _forHouseGoodsTypeT    = fieldNamed "goods_type",
                      _forHouseTitleT        = fieldNamed "title",
                      _forHouseDescriptionT  = fieldNamed "description",
                      _forHousePriceT        = fieldNamed "price",
                      -- _forHouseImageNamesT   = fieldNamed "image_names",
                      _forHouseVideoUrlT     = fieldNamed "video_url",
                      _forHouseAddrRegionT   = fieldNamed "addr_region",
                      -- _forHouseAddrAreaT  = fieldNamed "addr_area",
                      _forHouseAddrCityT     = fieldNamed "addr_city",
                      _forHouseAddrPointT    = fieldNamed "addr_point",
                      _forHouseAddrStreetT   = fieldNamed "addr_street",
                      _forHouseAddrHouseT    = fieldNamed "addr_house",
                      _forHouseContactPhoneT = fieldNamed "contact_phone"
                    }
  }

AvitoDb 
  (TableLens testTable)
  (TableLens forHouse) = dbLenses   

  

instance Table TestTableT where
  data PrimaryKey TestTableT f = TestTableId (Columnar f Int32) deriving Generic
  primaryKey = TestTableId . _testTableId 

type TestTableId = PrimaryKey TestTableT Identity

instance Beamable (PrimaryKey TestTableT)

instance Beamable TestTableT

TestTable
  (LensFor testTableId)    
  (LensFor testTableCol1)
  (LensFor testTableCol2)
  (LensFor testTableCol3) = tableLenses

getTestTableById id = do
  ps <- runSelectReturningList $ lookup_ (avitoDb ^. testTable) (TestTableId id)
  pure $ if List.length ps > 0 then Just $ List.head ps else Nothing

getTestTable :: Pg.Pg [TestTable]
getTestTable = do
  ps <- runSelectReturningList $ select $ all_ (avitoDb ^. testTable)  
  pure ps

replaceTestTableWith ts = do
  runDelete $ Database.Beam.delete (avitoDb ^. testTable) (const $ val_ True)
  runInsert $ Database.Beam.insert (avitoDb ^. testTable) (insertValues ts)

instance Table ForHouseT where
  data PrimaryKey ForHouseT f = ForHouseId (Columnar f Int32) deriving Generic
  primaryKey = ForHouseId . _forHouseIdT 

type ForHouseId = PrimaryKey ForHouseT Identity

instance Beamable (PrimaryKey ForHouseT)

instance Beamable ForHouseT

ForHouse
  (LensFor forHouseId)    
  (LensFor forHouseOid)
  (LensFor forHouseCategory)
  (LensFor forHouseGoodsType)
  (LensFor forHouseTitle)
  (LensFor forHouseDescription)
  (LensFor forHousePrice)
  -- (LensFor forHouseImageNames)
  (LensFor forHouseVideoUrl)
  (LensFor forHouseAddrRegion)
  -- (LensFor forHouseAddrArea)
  (LensFor forHouseAddrCity)
  (LensFor forHouseAddrPoint)
  (LensFor forHouseAddrStreet)
  (LensFor forHouseAddrHouse)
  (LensFor forHouseContactPhone) = tableLenses  

getForHouseById id = do
  ps <- runSelectReturningList $ lookup_ (avitoDb ^. forHouse) (ForHouseId id)
  pure $ if List.length ps > 0 then Just $ List.head ps else Nothing

getForHouse :: Pg.Pg [ForHouse]
getForHouse = do
  ps <- runSelectReturningList $ select $ all_ (avitoDb ^. forHouse)  
  pure ps

replaceForHouseWith ts = do
  runDelete $ Database.Beam.delete (avitoDb ^. forHouse) (const $ val_ True)
  runInsert $ Database.Beam.insert (avitoDb ^. forHouse) (insertValues ts)

data ForHouseA
  = ForHouseA {
    _forHouseId           :: Int32,
    _forHouseOid          :: Text,
    _forHouseCategory     :: Maybe Text,
    _forHouseGoodsType    :: Maybe Text,
    _forHouseTitle        :: Maybe Text,
    _forHouseDescription  :: Maybe Text,
    _forHousePrice        :: Maybe Text,
    _forHouseImageUrl     :: [(Text, Text)],
    _forHouseVideoUrl     :: Maybe Text,
    _forHouseAddrRegion   :: Maybe Text,
    -- _forHouseAddrArea   :: Columnar f (Maybe Text),
    _forHouseAddrCity     :: Maybe Text,
    _forHouseAddrPoint    :: Maybe Text,
    _forHouseAddrStreet   :: Maybe Text,
    _forHouseAddrHouse    :: Maybe Text,
    _forHouseContactPhone :: Maybe Text
  } 
  deriving (Generic, Show)

instance ToJSON (ForHouseA)
instance FromJSON (ForHouseA)

forHouseToA :: ForHouseT Identity -> IO (Either MinioErr ForHouseA)
forHouseToA h = do 
  urls <- getFileUrls (h ^. forHouseOid) 
  pure $ fmap (f h) urls
  where f :: ForHouseT Identity -> [(Text, Text)] -> ForHouseA
        f h1 urls = 
          ForHouseA {
            _forHouseId           = h1 ^. forHouseId
          , _forHouseOid          = h1 ^. forHouseOid
          , _forHouseCategory     = h1 ^. forHouseCategory
          , _forHouseGoodsType    = h1 ^. forHouseGoodsType
          , _forHouseTitle        = h1 ^. forHouseTitle
          , _forHouseDescription  = h1 ^. forHouseDescription
          , _forHousePrice        = h1 ^. forHousePrice
          , _forHouseImageUrl     = urls
          , _forHouseVideoUrl     = h1 ^. forHouseVideoUrl
          , _forHouseAddrRegion   = h1 ^. forHouseAddrRegion
           -- _forHouseAddrArea  f (Maybe Text),
          , _forHouseAddrCity     = h1 ^. forHouseAddrCity
          , _forHouseAddrPoint    = h1 ^. forHouseAddrPoint
          , _forHouseAddrStreet   = h1 ^. forHouseAddrStreet
          , _forHouseAddrHouse    = h1 ^. forHouseAddrHouse
          , _forHouseContactPhone = h1 ^. forHouseContactPhone
          }

forHouseFromA :: ForHouseA -> ForHouse
forHouseFromA a = ForHouse {
      _forHouseIdT           = _forHouseId a
    , _forHouseOidT          = _forHouseOid a
    , _forHouseCategoryT     = _forHouseCategory  a
    , _forHouseGoodsTypeT    = _forHouseGoodsType a
    , _forHouseTitleT        = _forHouseTitle a
    , _forHouseDescriptionT  = _forHouseDescription a
    , _forHousePriceT        = _forHousePrice a
    , _forHouseVideoUrlT     = _forHouseVideoUrl a
    , _forHouseAddrRegionT   = _forHouseAddrRegion a
    , _forHouseAddrCityT     = _forHouseAddrCity a
    , _forHouseAddrPointT    = _forHouseAddrPoint a
    , _forHouseAddrStreetT   = _forHouseAddrStreet a
    , _forHouseAddrHouseT    = _forHouseAddrHouse a
    , _forHouseContactPhoneT = _forHouseContactPhone a
}
