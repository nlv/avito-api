{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Meta (
  TName,
  TNameT(..),
  TNameId,
  TField,
  TFieldT(..),
  TFieldId,
  -- Opt,
  -- OptT(..),
  -- OptId,  
  OptsVal,
  OptsValT(..),
  OptsValId
  )  where

import GHC.Generics
import Data.Aeson
import Data.Text
import Data.Int
import Database.Beam
import Database.Beam.Postgres

data TNameT f
  = TName {
    _tnameIdT           :: Columnar f Int32
  , _tnameTnameT        :: Columnar f Text
  , _tnameLabelT        :: Columnar f Text
  }
  deriving Generic

type TName = TNameT Identity

deriving instance Show TName
deriving instance Eq TName

type TNameId = PrimaryKey TNameT Identity

data TFieldT f
  = TField {
    _tfieldIdT          :: Columnar f Int32
  , _tfieldTnameT       :: Columnar f Text
  , _tfieldNameT        :: Columnar f Text
  , _tfieldLabelT       :: Columnar f Text
  , _tfieldTypeT        :: Columnar f Text
  , _tfieldOptnameT     :: Columnar f (Maybe Text)
  }
  deriving Generic

type TField = TFieldT Identity

deriving instance Show TField
deriving instance Eq TField

type TFieldId = PrimaryKey TFieldT Identity

-- data OptsListT f
--   = OptsList {
--     _optsListIdT   :: Columnar f Int32
--   , _optsListNameT :: Columnar f Text
--   }
--   deriving Generic

-- type OptsList = OptsListT Identity

-- deriving instance Show OptsList
-- deriving instance Eq OptsList

-- type OptsListId = PrimaryKey OptsListT Identity

data OptsValT f
  = OptsVal {
    _optsValIdT      :: Columnar f Int32
  , _optsValOptnameT :: Columnar f Text
  , _optsValLabelT   :: Columnar f Text
  , _optsValValT     :: Columnar f Text
  }
  deriving Generic

type OptsVal = OptsValT Identity

deriving instance Show OptsVal
deriving instance Eq OptsVal

type OptsValId = PrimaryKey OptsValT Identity

