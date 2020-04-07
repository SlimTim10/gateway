module Encoding
  -- (
  -- )
  where

import qualified Data.ByteString.Char8 as B
import Data.List.Split (splitOn)
import Data.Char (chr)

encode :: B.ByteString -> B.ByteString
encode = encodeCOBS

encodeCOBS :: B.ByteString -> B.ByteString
encodeCOBS = B.concat . map enc . chunks
  where
    chunks :: B.ByteString -> [B.ByteString]
    chunks = B.split '\0'
    enc :: B.ByteString -> B.ByteString
    enc chunk = chr (B.length chunk + 1) `B.cons` chunk
