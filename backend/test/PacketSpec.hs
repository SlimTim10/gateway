module PacketSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Data.Either (isLeft, isRight)

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
