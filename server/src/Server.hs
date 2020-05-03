module Server
  ( checkTrigger
  , applyAction
  , sendPacket
  , runRules
  , handleRawPacket
  ) where

import Data.List (foldl')
import System.Hardware.Serialport (SerialPort)
import Control.Monad
  ( foldM
  , void
  )

import Types.Rule (Rule(..))
import Types.Rule.Trigger
  ( Trigger
  , TriggerElement(..)
  )
import Types.Rule.Action
  ( Action
  , ActionElement(..)
  )
import State
  ( State
  , (!?)
  )
import Types.Prop (Prop(..))
import qualified Types.Prop as Prop
import Packet
  ( Packet(..)
  , RawPacket
  )
import qualified Packet
import ReliableSerial (sendRawPacket)
import qualified Command as Cmd
import qualified State
import Rules (Rules)

checkTrigger :: State -> Trigger -> Bool
checkTrigger state = all (checkTriggerElement state)

checkTriggerElement :: State -> TriggerElement -> Bool
checkTriggerElement
  state
  TriggerElement { address = addr, value = tv }
  =
  case state !? addr of
    Nothing -> False
    Just prop -> Prop.value prop == tv

applyAction :: State -> Action -> State
applyAction = foldl' applyActionElement

applyActionElement :: State -> ActionElement -> State
applyActionElement
  state
  ActionElement { address = addr, value = av }
  =
  State.update f addr state
  where
    f prop = Just $ (prop :: Prop) { value = av }

handleRawPacket :: State -> RawPacket -> State
handleRawPacket state raw = case Packet.fromRaw raw of
  Left e -> error $ "Invalid packet: " ++ e
  Right packet -> handlePacket state packet

handlePacket :: State -> Packet -> State
handlePacket
  state
  Packet { propAddress = addr, commandID = cmd, payload }
  =
  case cmd of
    Cmd.Ping -> error "Not yet supported"
    _ -> State.update f (fromIntegral addr) state
  where
    f prop = Just $ (prop :: Prop) { value = payload }

sendPacket :: SerialPort -> Packet -> IO (Int)
sendPacket serial packet = sendRawPacket serial (Packet.toRaw packet)

sendPacket_ :: SerialPort -> Packet -> IO ()
sendPacket_ serial packet = void $ sendRawPacket serial (Packet.toRaw packet)

runActionElement :: SerialPort -> State -> ActionElement -> IO (State)
runActionElement
  serial
  state
  ae@(ActionElement { address = addr', value = v })
  = do
  let
    cmd = case v of
      Prop.Int _ -> Cmd.PayloadInt
      Prop.IntList _ -> Cmd.PayloadIntList
      Prop.String _ -> Cmd.PayloadString
      _ -> error "Invalid value in action"
    addr = case state !? addr' of
      Just p -> Prop.address p
      Nothing -> error "Invalid prop address in action"
    packet = Packet
      { propAddress = addr
      , commandID = cmd
      , payload = v
      }
  sendPacket_ serial packet
  let state' = applyActionElement state ae
  return state'

runAction :: SerialPort -> State -> Action -> IO (State)
runAction serial = foldM (runActionElement serial)

runRules :: SerialPort -> State -> Rules -> IO (State)
runRules
  serial
  state
  = foldM (runAction serial) state
  . map action
  . filter (checkTrigger state . trigger)
