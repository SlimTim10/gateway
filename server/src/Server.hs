module Server
  ( checkTrigger
  , applyAction
  , sendPacket
  , runRules
  , handleRawPacket
  , handlePacket
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
import Types.Packet
  ( Packet(..)
  , RawPacket
  , PacketException(..)
  )
import qualified Types.Packet as Packet
import ReliableSerial (sendRawPacket)
import qualified Types.Command as Cmd
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

handleRawPacket :: State -> RawPacket -> Either PacketException State
handleRawPacket state raw = do
  packet <- Packet.fromRaw raw
  handlePacket state packet

-- TODO: replace error with exception handling
handlePacket :: State -> Packet -> Either PacketException State
handlePacket
  state
  Packet { propAddress = addr, commandID = cmd, payload = Nothing }
  | addr `State.notMember` state = Left $ InvalidPropAddress addr
  | otherwise = case cmd of
      Cmd.Ping -> error "Ping command not yet supported"
      _ -> error "No command"
handlePacket
  state
  Packet { propAddress = addr, commandID = cmd, payload = Just pld }
  | addr `State.notMember` state = Left $ InvalidPropAddress addr
  | otherwise = case cmd of
      Cmd.Ping -> error "Ping command not yet supported"
      _ -> Right $ State.update f addr state
  where
    f prop = Just $ (prop :: Prop) { value = pld }

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
    addr = case state !? addr' of
      Just p -> Prop.address p
      -- TODO: replace error with exception handling
      Nothing -> error "Invalid prop address in action"
    packet = Packet
      { propAddress = addr
      , commandID = cmd
      , payload = Just v
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
