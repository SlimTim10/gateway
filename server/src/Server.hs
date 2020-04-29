module Server
  ( checkTrigger
  , applyAction
  , sendPacket
  ) where

import Data.List (foldl')
import System.Hardware.Serialport (SerialPort)

import Types.Rule
  -- ( Rule(..)
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
import Packet (Packet(..))
import qualified Packet
import ReliableSerial (sendRawPacket)
import qualified Command as Cmd

checkTrigger :: State -> Trigger -> Bool
checkTrigger state = all (checkTriggerElement state)

checkTriggerElement :: State -> TriggerElement -> Bool
checkTriggerElement
  state
  TriggerElement { propKey = key, value = tv }
  =
  case state !? key of
    Nothing -> False
    Just prop -> Prop.value prop == tv

applyAction :: State -> Action -> State
applyAction = foldl' applyActionElement

applyActionElement :: State -> ActionElement -> State
applyActionElement
  state
  ActionElement { propKey = key, value = av }
  =
  State.update f key state
  where
    f prop = Just $ (prop :: Prop) { value = av }

sendPacket :: SerialPort -> Packet -> IO (Int)
sendPacket serial packet = sendRawPacket serial (Packet.toRaw packet)

-- executeRule :: State -> Rule -> IO (State)
-- executeRule
--   state
--   Rule { trigger = trg, action = act }
--   | checkTrigger state trg = return state
--   | otherwise = return state

executeActionElement :: SerialPort -> State -> ActionElement -> IO (State)
executeActionElement
  serial
  state
  ae@(ActionElement { propKey = key, value = v })
  = do
  let
    cmd = case v of
      Prop.Int _ -> Cmd.PayloadInt
      Prop.IntList _ -> Cmd.PayloadIntList
      Prop.String _ -> Cmd.PayloadString
      _ -> error "Invalid value in action"
    addr = case state !? key of
      Just p -> address p
      Nothing -> error "Invalid prop key in action"
    packet = Packet
      { propAddress = addr
      , commandID = cmd
      , payload = v
      }
  _ <- sendPacket serial packet
  let state' = applyActionElement state ae
  return state'

-- executeRules :: State -> Rules -> IO (State)
-- executeRules state rules = do
--   sendRawPacket
