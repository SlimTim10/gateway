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
    it "encodes a list of bytes" $ do
      let xs = fromBytes [0x45, 0x00, 0x00, 0x2C, 0x4C, 0x79, 0x00, 0x00, 0x40, 0x06, 0x4F, 0x37]
      let ys = fromBytes [0x02, 0x45, 0x01, 0x04, 0x2C, 0x4C, 0x79, 0x01, 0x05, 0x40, 0x06, 0x4F, 0x37]
      encodeCOBS xs `shouldBe` ys

    it "encodes a raw packet" $ do
      let xs = fromBytes [0x00, 0x00, 0x00, 0x01, 0x01, 0x03]
      let ys = fromBytes [0x01, 0x01, 0x01, 0x04, 0x01, 0x01, 0x03]
      encodeCOBS xs `shouldBe` ys

    it "encodes a list of bytes of maximum length" $ do
      let xs = fromBytes $ replicate encodeCOBSMaxLength 0x01
      let ys = fromBytes $ 0xFF : replicate (decodeCOBSMaxLength - 1) 0x01
      encodeCOBS xs `shouldBe` ys

    it "truncates when encoding a list of bytes longer than the maximum length" $ do
      let xs = fromBytes $ replicate (encodeCOBSMaxLength + 1) 0x01
      let ys = fromBytes $ 0xFF : replicate (decodeCOBSMaxLength - 1) 0x01
      encodeCOBS xs `shouldBe` ys

  describe "decodeCOBS" $ do
    it "decodes a list of bytes" $ do
      let xs = fromBytes [0x02, 0x45, 0x01, 0x04, 0x2C, 0x4C, 0x79, 0x01, 0x05, 0x40, 0x06, 0x4F, 0x37]
      let ys = fromBytes [0x45, 0x00, 0x00, 0x2C, 0x4C, 0x79, 0x00, 0x00, 0x40, 0x06, 0x4F, 0x37]
      decodeCOBS xs `shouldBe` ys

    it "decodes a raw packet" $ do
      let xs = fromBytes [0x01, 0x01, 0x01, 0x04, 0x01, 0x01, 0x03]
      let ys = fromBytes [0x00, 0x00, 0x00, 0x01, 0x01, 0x03]
      decodeCOBS xs `shouldBe` ys

    it "decodes a list of bytes of maximum length" $ do
      let xs = fromBytes $ 0xFF : replicate (decodeCOBSMaxLength - 1) 0x01
      let ys = fromBytes $ replicate encodeCOBSMaxLength 0x01
      decodeCOBS xs `shouldBe` ys

    it "truncates when decoding a list of bytes longer than the maximum length" $ do
      let xs = fromBytes $ 0xFF : replicate (decodeCOBSMaxLength + 1) 0x01
      let ys = fromBytes $ replicate encodeCOBSMaxLength 0x01
      decodeCOBS xs `shouldBe` ys
