module Rules
  -- (
  -- ) where
  where

-- import qualified Data.IntMap.Strict as IntMap

import State
  ( State
  )
import Types.Rule
  ( Rule(..)
  -- , Trigger(..)
  -- , Action(..)
  )
-- import qualified Types.Prop as Prop
import Config
  ( ConfigRule(..)
  )

type Rules = [Rule]

fromConfig :: State -> [ConfigRule] -> Either String Rules
fromConfig = undefined
-- fromConfig state config = Right $ rs
--   where
--     rs = []
