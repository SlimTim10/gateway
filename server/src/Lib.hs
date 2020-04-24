module Lib where

import qualified Data.ByteString.Lazy as B
import Data.Aeson (eitherDecode)
import Data.Aeson.Types (FromJSON)

readJSON :: FromJSON a => FilePath -> IO (Either String a)
readJSON fp = do
  bs <- B.readFile fp
  return $ eitherDecode bs
