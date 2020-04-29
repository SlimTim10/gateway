module Server
  ( checkTrigger
  , applyAction
  , sendPacket
  ) where

import Data.List (foldl')
import System.Hardware.Serialport (SerialPort)

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
import Packet (Packet)
import qualified Packet
import ReliableSerial (sendRawPacket)

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

sendPacket :: SerialPort -> Packet -> IO (Int)
sendPacket s p = sendRawPacket s (Packet.toRaw p)

-- runRules :: State -> Rules -> IO (State)
-- runRules state rules = do
--   sendRawPacket
