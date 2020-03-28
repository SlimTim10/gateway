module PacketSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Data.Either (isLeft, isRight)
import qualified Data.ByteString.Char8 as B
import Data.Char (chr)

import Packet

spec :: Spec
spec = do
  describe "readPacket" $ do
    it "returns a single Packet" $ do
      contents <- readPacket "test/data/packet.json"
      contents `shouldSatisfy` isRight
      let (Right pkt) = contents
      pkt `shouldBe`
        Packet
        { propAddress = 1
        , payload = [3, 2, 1]
        }

  describe "fromBytes" $ do
    it "returns a valid Packet" $ do
      let bs = [0x00, 0x00, 0x00, 0x01, 0x01, 0x03]
      let p = fromBytes $ B.pack . map chr $ bs
      p `shouldSatisfy` isRight
      let (Right pkt) = p
      pkt `shouldBe` 
        Packet
        { propAddress = 1
        , payload = [3]
        }

    it "returns a valid Packet with a larger payload" $ do
      let bs = [0x00, 0x00, 0x00, 0x01, 0x03, 0x03, 0x02, 0x01]
      let p = fromBytes $ B.pack . map chr $ bs
      p `shouldSatisfy` isRight
      let (Right pkt) = p
      pkt `shouldBe` 
        Packet
        { propAddress = 1
        , payload = [3, 2, 1]
        }

    it "returns an error message when the packet is too small" $ do
      let bs = [0x00, 0x00, 0x01]
      let p = fromBytes $ B.pack . map chr $ bs
      p `shouldSatisfy` isLeft

    it "returns an error message when the payload does not match the length specified in the header" $ do
      let bs = [0x00, 0x00, 0x00, 0x01, 0x02, 0x01]
      let p = fromBytes $ B.pack . map chr $ bs
      p `shouldSatisfy` isLeft
