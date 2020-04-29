module Server
  ( checkTrigger
  , applyAction
  ) where

import Data.List (foldl')

import Types.Rule
  ( TriggerElement(..)
  , Trigger
  , ActionElement(..)
  , Action
  )
import State
  ( State
  , (!?)
  )
import qualified State
import Types.Prop (Prop(..))
import qualified Types.Prop as Prop

checkTrigger :: State -> Trigger -> Bool
checkTrigger state = all (checkTriggerElement state)

checkTriggerElement :: State -> TriggerElement -> Bool
checkTriggerElement
  state
  ( TriggerElement { propKey = key, value = tv } )
  =
  case state !? key of
    Nothing -> False
    Just prop -> Prop.value prop == tv

applyAction :: State -> Action -> State
applyAction = foldl' applyActionElement

applyActionElement :: State -> ActionElement -> State
applyActionElement
  state
  ( ActionElement { propKey = key, value = av } )
  =
  State.update f key state
  where
    f prop = Just $ (prop :: Prop) { value = av }
