{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-#  LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Base (
  getTestTableByName
  )  where

import GHC.Generics
import Database.Beam
import Data.Text as Text
import Data.List as List
import Data.Int
import Data.Maybe
import Lens.Micro
import Data
import System.IO

instance Beamable TestTableT

instance Table TestTableT where
  data PrimaryKey TestTableT f = TestTableId (Columnar f Int32) deriving Generic
  primaryKey = TestTableId . _testTableId
instance Beamable (PrimaryKey TestTableT)

data AvitoDb f = AvitoDb
                      { _testTable :: f (TableEntity TestTableT) 
                      }
                        deriving Generic

instance Database be AvitoDb

avitoDb :: DatabaseSettings be AvitoDb
avitoDb = defaultDbSettings `withDbModification`
  dbModification {
    _testTable = setEntityName "test_table" <>
                  modifyTableFields tableModification {
                    _testTableId   = fieldNamed "id",
                    _testTableName = fieldNamed "name",
                    _testTableCol1 = fieldNamed "col1",
                    _testTableCol2 = fieldNamed "col2",
                    _testTableCol3 = fieldNamed "col3"
                  }
  }

TestTable
  (LensFor testTableId)    
  (LensFor testTableName)
  (LensFor testTableCol1)
  (LensFor testTableCol2)
  (LensFor testTableCol3) = tableLenses

AvitoDb 
  (TableLens testTable) = dbLenses 

getTestTableByName name = do
  ps <- runSelectReturningList $ select $ do
        t <- all_ (avitoDb ^. testTable)
        guard_ (t ^. testTableName ==. val_ name)
        pure t
  pure $ if List.length ps > 0 then Just $ List.head ps else Nothing
