{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}



module DataTestTable (
  TestTable,
  TestTableT(..)
  -- ,
  -- TestTableId(..)
  )  where

import GHC.Generics
import Data.Aeson
import Data.Text
import Data.Int
import Database.Beam

data TestTableT f
  = TestTable {
    _testTableId     :: Columnar f Int32,
    _testTableCol1   :: Columnar f (Maybe Text),
    _testTableCol2   :: Columnar f (Maybe Text),
    _testTableCol3   :: Columnar f (Maybe Text)
  }
  deriving Generic

type TestTable = TestTableT Identity

deriving instance Show TestTable
deriving instance Eq TestTable

instance ToJSON (TestTableT Identity)
instance FromJSON (TestTableT Identity)


