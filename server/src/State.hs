module State
  -- (
  -- ) where
  where

import Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap

import State.Prop (Prop)
import Config (Config)

type State = IntMap Prop

fromConfig :: Config -> State
fromConfig = undefined
