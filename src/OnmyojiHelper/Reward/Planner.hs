{-# LANGUAGE RankNTypes #-}

module OnmyojiHelper.Reward.Planner
  ( simple
  ) where

import           Control.Lens                 hiding (simple)
import           Data.Map.Strict              (Map)
-- import qualified Data.Map.Strict              as Map
import           Data.Text                    (Text)

import           OnmyojiHelper.Reward.Dungeon

-- |求出在最小开销的前提下，每个关卡应该打多少次
type Planner = [Dungeon] -- ^关卡列表
            -> [ReifiedLens' DungeonCost Int] -- ^越靠前的开销，越优先降低
            -> Map Text Int -- ^每种怪物最小数量
            -- |(关卡名到数量的Map, 实际开销)
            -> (Map Text Int, DungeonCost)

simple :: Planner
simple _ _ = undefined
