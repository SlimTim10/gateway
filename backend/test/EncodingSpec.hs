module EncodingSpec (spec) where

import Test.Hspec
import Data.Char (chr)
import qualified Data.ByteString.Char8 as B

import Encoding

fromBytes :: [Int] -> B.ByteString
fromBytes = B.pack . map chr

spec :: Spec
spec = do
  describe "encodeCOBS" $ do
    it "encodes a list of integers" $ do
      let xs = fromBytes [0x07, 0x09, 0x00, 0x01, 0x00, 0x00, 0x02, 0x03, 0x04, 0x05, 0x06, 0x00, 0x18, 0x22]
      let ys = fromBytes [0x03, 0x07, 0x09, 0x02, 0x01, 0x01, 0x06, 0x02, 0x03, 0x04, 0x05, 0x06, 0x03, 0x18, 0x22]
      encodeCOBS xs `shouldBe` ys

    it "encodes a raw packet" $ do
      let xs = fromBytes [0x00, 0x00, 0x00, 0x01, 0x01, 0x03]
      let ys = fromBytes [0x01, 0x01, 0x01, 0x04, 0x01, 0x01, 0x03]
      encodeCOBS xs `shouldBe` ys

  describe "decodeCOBS" $ do
    it "decodes a list of integers" $ do
      let xs = fromBytes [0x03, 0x07, 0x09, 0x02, 0x01, 0x01, 0x06, 0x02, 0x03, 0x04, 0x05, 0x06, 0x03, 0x18, 0x22]
      let ys = fromBytes [0x07, 0x09, 0x00, 0x01, 0x00, 0x00, 0x02, 0x03, 0x04, 0x05, 0x06, 0x00, 0x18, 0x22]
      decodeCOBS xs `shouldBe` ys

    it "decodes a raw packet" $ do
      let xs = fromBytes [0x01, 0x01, 0x01, 0x04, 0x01, 0x01, 0x03]
      let ys = fromBytes [0x00, 0x00, 0x00, 0x01, 0x01, 0x03]
      decodeCOBS xs `shouldBe` ys
