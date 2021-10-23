{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}


module DataForHouse (
  ForHouse,
  ForHouseT(..),
  ForHouseId
  )  where

import GHC.Generics
import Data.Aeson
import Data.Text
import Data.Int
import Database.Beam

data ForHouseT f
  = ForHouse {
    _forHouseId     :: Columnar f Int32,
    _forHouseOid   :: Columnar f Text,
    _forHouseCategory   :: Columnar f (Maybe Text),
    _forHouseGoodsType   :: Columnar f (Maybe Text),
    _forHouseTitle   :: Columnar f (Maybe Text),
    _forHouseDescription   :: Columnar f (Maybe Text),
    _forHousePrice   :: Columnar f (Maybe Text),
    _forHouseImageNames   :: Columnar f (Maybe Text),
    _forHouseVideoUrl   :: Columnar f (Maybe Text),
    _forHouseAddrRegion   :: Columnar f (Maybe Text),
    -- _forHouseAddrArea   :: Columnar f (Maybe Text),
    _forHouseAddrCity   :: Columnar f (Maybe Text),
    _forHouseAddrPoint   :: Columnar f (Maybe Text),
    _forHouseAddrStreet   :: Columnar f (Maybe Text),
    _forHouseAddrHouse   :: Columnar f (Maybe Text),
    _forHouseContactPhone   :: Columnar f (Maybe Text)
  }
  deriving Generic



type ForHouse = ForHouseT Identity

deriving instance Show ForHouse
deriving instance Eq ForHouse

instance ToJSON (ForHouseT Identity)
instance FromJSON (ForHouseT Identity)

type ForHouseId = PrimaryKey ForHouseT Identity


