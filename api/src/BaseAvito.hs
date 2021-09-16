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

  , getForHouseById
  , getForHouse
  , replaceForHouseWith  
  )  where

import GHC.Generics
import Database.Beam
import Database.Beam.Backend.SQL
import qualified Database.Beam.Postgres as Pg
import Data.List as List
import Data.Int
import Lens.Micro
import DataTestTable
import DataForHouse

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
                      _forHouseId     = fieldNamed "id",
                      _forHouseCategory   = fieldNamed "category",
                      _forHouseGoodsType   = fieldNamed "goods_type",
                      _forHouseTitle   = fieldNamed "title",
                      _forHouseDescription   = fieldNamed "description",
                      _forHousePrice   = fieldNamed "price",
                      _forHouseImageNames   = fieldNamed "image_names",
                      _forHouseVideoUrl   = fieldNamed "video_url",
                      _forHouseAddrRegion   = fieldNamed "addr_region",
                      -- _forHouseAddrArea   = fieldNamed "addr_area",
                      _forHouseAddrCity   = fieldNamed "addr_city",
                      _forHouseAddrPoint   = fieldNamed "addr_point",
                      _forHouseAddrStreet   = fieldNamed "addr_street",
                      _forHouseAddrHouse   = fieldNamed "addr_house",
                      _forHouseContactPhone   = fieldNamed "contact_phone"
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
  primaryKey = ForHouseId . _forHouseId 

type ForHouseId = PrimaryKey ForHouseT Identity

instance Beamable (PrimaryKey ForHouseT)

instance Beamable ForHouseT

ForHouse
  (LensFor forHouseId)    
  (LensFor forHouseCategory)
  (LensFor forHouseGoodsType)
  (LensFor forHouseTitle)
  (LensFor forHouseDescription)
  (LensFor forHousePrice)
  (LensFor forHouseImageNames)
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


