module PacketSpec (spec) where

import Test.Hspec
import Packet

import Data.Either (isLeft, isRight)
import qualified Data.ByteString.Char8 as B
import Data.Char (chr)

import qualified Types.Prop as Prop
import qualified Command as Cmd

spec :: Spec
spec = do
  describe "readPacket" $ do
    it "parses a single packet from a JSON file" $ do
      True `shouldBe` True
      contents <- readPacket "test/data/packet.json"
      contents `shouldSatisfy` isRight
      let (Right pkt) = contents
      pkt `shouldBe`
        Packet
        { propAddress = 1
        , commandID = Cmd.PayloadInt
        , payload = Prop.Int 1
        }

  describe "fromBytes" $ do
    it "parses a packet with an integer payload from bytes" $ do
      let bs = [0x00, 0x00, 0x00, 0x01, 0x01, 0x01]
      let p = fromBytes $ B.pack . map chr $ bs
      p `shouldSatisfy` isRight
      let (Right pkt) = p
      pkt `shouldBe` 
        Packet
        { propAddress = 1
        , commandID = Cmd.PayloadInt
        , payload = Prop.Int 1
        }

    it "parses a packet with an integer list payload from bytes" $ do
      let bs = [0x00, 0x00, 0x00, 0x01, 0x02, 0x01, 0x02, 0x03]
      let p = fromBytes $ B.pack . map chr $ bs
      p `shouldSatisfy` isRight
      let (Right pkt) = p
      pkt `shouldBe` 
        Packet
        { propAddress = 1
        , commandID = Cmd.PayloadIntList
        , payload = Prop.IntList [1, 2, 3]
        }

    it "parses a packet with a string payload from bytes" $ do
      let bs = [0x00, 0x00, 0x00, 0x01, 0x03, 0x63, 0x6C, 0x6F, 0x73, 0x65, 0x64]
      let p = fromBytes $ B.pack . map chr $ bs
      p `shouldSatisfy` isRight
      let (Right pkt) = p
      pkt `shouldBe` 
        Packet
        { propAddress = 1
        , commandID = Cmd.PayloadString
        , payload = Prop.String "closed"
        }

    it "parses a packet with a ping command from bytes" $ do
      let bs = [0x00, 0x00, 0x00, 0x01, 0x80]
      let p = fromBytes $ B.pack . map chr $ bs
      p `shouldSatisfy` isRight
      let (Right pkt) = p
      pkt `shouldBe` 
        Packet
        { propAddress = 1
        , commandID = Cmd.Ping
        , payload = Prop.Nothing
        }

    it "returns an error message when the packet is too small" $ do
      let bs = [0x00, 0x00, 0x01]
      let p = fromBytes $ B.pack . map chr $ bs
      p `shouldSatisfy` isLeft

    it "returns an error message when the payload of an single integer is more than one byte" $ do
      let bs = [0x00, 0x00, 0x00, 0x01, 0x01, 0x01, 0x02]
      let p = fromBytes $ B.pack . map chr $ bs
      p `shouldSatisfy` isLeft

    it "returns an error message when the payload is empty when it should not be" $ do
      let bs1 = [0x00, 0x00, 0x00, 0x01, 0x01]
      let p1 = fromBytes $ B.pack . map chr $ bs1
      let bs2 = [0x00, 0x00, 0x00, 0x01, 0x02]
      let p2 = fromBytes $ B.pack . map chr $ bs2
      let bs3 = [0x00, 0x00, 0x00, 0x01, 0x03]
      let p3 = fromBytes $ B.pack . map chr $ bs3
      p1 `shouldSatisfy` isLeft
      p2 `shouldSatisfy` isLeft
      p3 `shouldSatisfy` isLeft
