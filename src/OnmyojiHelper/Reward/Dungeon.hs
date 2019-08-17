{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module OnmyojiHelper.Reward.Dungeon
  ( Dungeon(..)
  , dungeon_name
  , dungeon_cost
  , dungeon_elves
  , DungeonCost(..)
  , dungeonCost_energy
  , dungeonCost_round
  , DungeonElf(..)
  , dungeonElf_name
  , dungeonElf_count
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

import           OnmyojiHelper.Util (removeNsOptions)

data Dungeon = Dungeon
  { _dungeon_name  :: DungeonName -- ^关卡名称
  , _dungeon_cost  :: DungeonCost -- ^开销
  , _dungeon_elves :: [DungeonElf] -- ^关卡内包含的怪物
  } deriving (Show, Eq, Generic)

data DungeonCost = DungeonCost
  { _dungeonCost_energy :: Int -- ^体力
   ,_dungeonCost_round  :: Int -- ^回目数
  } deriving (Show, Eq, Generic)

data DungeonElf = DungeonElf
  { _dungeonElf_name  :: ElfName -- ^怪物名称
  , _dungeonElf_count :: Int -- ^怪物数量
  } deriving (Show, Eq, Generic)

newtype DungeonName = DungeonName Text
  deriving (Show, Eq, FromJSON, ToJSON)

newtype ElfName = ElfName Text
  deriving (Show, Eq, FromJSON, ToJSON)

makeLenses ''Dungeon
makeLenses ''DungeonCost
makeLenses ''DungeonElf

instance FromJSON Dungeon where
  parseJSON = genericParseJSON removeNsOptions

instance ToJSON Dungeon where
  toEncoding = genericToEncoding removeNsOptions
  toJSON = genericToJSON removeNsOptions

instance FromJSON DungeonCost where
  parseJSON = genericParseJSON removeNsOptions

instance ToJSON DungeonCost where
  toEncoding = genericToEncoding removeNsOptions
  toJSON = genericToJSON removeNsOptions

instance FromJSON DungeonElf where
  parseJSON = genericParseJSON removeNsOptions

instance ToJSON DungeonElf where
  toEncoding = genericToEncoding removeNsOptions
  toJSON = genericToJSON removeNsOptions
