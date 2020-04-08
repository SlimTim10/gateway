module LibSpec (spec) where

import Test.Hspec

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.Either (isLeft)

import Lib

data Dummy = Dummy
  deriving (Show, Eq, Generic, FromJSON)

spec :: Spec
spec = do
  describe "readJSON" $ do
    it "throws an exception if used with a non-existent file" $ do
      (readJSON "noSuchFile.json" :: IO (Either String Dummy))
        `shouldThrow` anyIOException

    it "returns a JSON type with a valid file" $ do
      dummy <- readJSON "test/data/dummy.json"
      dummy `shouldBe` Right Dummy

    it "returns an error with an invalid file" $ do
      dummy <- (readJSON "test/data/dummyInvalid.json" :: IO (Either String Dummy))
      dummy `shouldSatisfy` isLeft
