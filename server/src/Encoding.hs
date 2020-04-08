module Encoding
  ( encode
  , decode
  , encodeCOBS
  , decodeCOBS
  )
  where

import qualified Data.ByteString.Char8 as B
import Data.Char (chr, ord)
import Data.List (unfoldr)

encode :: B.ByteString -> B.ByteString
encode = encodeCOBS

encodeCOBS :: B.ByteString -> B.ByteString
encodeCOBS = B.concat . map enc . chunks
  where
    chunks :: B.ByteString -> [B.ByteString]
    chunks = B.split '\0'
    enc :: B.ByteString -> B.ByteString
    enc chunk = chr (B.length chunk + 1) `B.cons` chunk

decode :: B.ByteString -> B.ByteString
decode = decodeCOBS

decodeCOBS :: B.ByteString -> B.ByteString
decodeCOBS = B.init . B.concat . map enc . chunks
  where
    enc :: B.ByteString -> B.ByteString
    enc chunk = B.tail chunk `B.snoc` chr 0x00
    chunks :: B.ByteString -> [B.ByteString]
    chunks = unfoldr $ \xs ->
      if B.null xs
      then Nothing
      else Just $ B.splitAt (ord $ B.head xs) xs
