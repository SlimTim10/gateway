module ReliableSerialSpec (spec) where

import Test.Hspec
import Data.Char (chr)
import qualified Data.ByteString.Char8 as B

import ReliableSerial

fromBytes :: [Int] -> B.ByteString
fromBytes = B.pack . map chr

spec :: Spec
spec = do
  describe "fletcher16" $ do
    it "returns an empty checksum for empty input" $ do
      fletcher16 B.empty `shouldBe` 0x0000

    it "calculates a checksum of 2 bytes" $ do
      let xs = fromBytes [0x01, 0x02]
      fletcher16 xs `shouldBe` 0x0403

    it "calculates some simple checksums" $ do
      fletcher16 (B.pack "abcde") `shouldBe` 0xC8F0
      fletcher16 (B.pack "abcdef") `shouldBe` 0x2057
      fletcher16 (B.pack "abcdefgh") `shouldBe` 0x0627

  describe "check" $ do
    it "returns true when the packet is empty" $ do
      check B.empty `shouldBe` True
      let xs = fromBytes [0x00, 0x00]
      check xs `shouldBe` True
      
    it "returns true when the check bytes at the end of the packet are correct" $ do
      let xs = fromBytes [0x01, 0x02, 0xF8, 0x04]
      check xs `shouldBe` True

    it "returns false when the check bytes at the end of the packet are incorrect" $ do
      let xs = fromBytes [0x01, 0x02, 0x00, 0x00]
      check xs `shouldBe` False

  describe "addCheckBytes" $ do
    it "adds check bytes to packets" $ do
      let xs = fromBytes [0x01, 0x02]
      let ys = fromBytes [0x01, 0x02, 0xF8, 0x04]
      addCheckBytes xs `shouldBe` ys
