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
    _forHouseIdT           :: Columnar f Int32,
    _forHouseOidT          :: Columnar f Text,
    _forHouseCategoryT     :: Columnar f (Maybe Text),
    _forHouseGoodsTypeT    :: Columnar f (Maybe Text),
    _forHouseTitleT        :: Columnar f (Maybe Text),
    _forHouseDescriptionT  :: Columnar f (Maybe Text),
    _forHousePriceT        :: Columnar f (Maybe Text),
    -- _forHouseImageNamesT   :: Columnar f (Maybe Text),
    _forHouseVideoUrlT     :: Columnar f (Maybe Text),
    _forHouseAddrRegionT   :: Columnar f (Maybe Text),
    -- _forHouseAddrAreaT  :: Columnar f (Maybe Text),
    _forHouseAddrCityT     :: Columnar f (Maybe Text),
    _forHouseAddrPointT    :: Columnar f (Maybe Text),
    _forHouseAddrStreetT   :: Columnar f (Maybe Text),
    _forHouseAddrHouseT    :: Columnar f (Maybe Text),
    _forHouseContactPhoneT :: Columnar f (Maybe Text)
  }
  deriving Generic



type ForHouse = ForHouseT Identity

deriving instance Show ForHouse
deriving instance Eq ForHouse

instance ToJSON (ForHouseT Identity)
instance FromJSON (ForHouseT Identity)

type ForHouseId = PrimaryKey ForHouseT Identity



