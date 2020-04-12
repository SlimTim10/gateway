module Encoding
  ( cobsEncode
  , cobsDecode
  , cobsEncodeMaxLength
  , cobsDecodeMaxLength
  , cobsBoundary
  )
  where

import qualified Data.ByteString.Char8 as B
import Data.Char (chr, ord)
import Data.List (unfoldr)

cobsEncodeMaxLength :: Int
cobsEncodeMaxLength = 254

cobsDecodeMaxLength :: Int
cobsDecodeMaxLength = 255

cobsBoundary :: B.ByteString
cobsBoundary = B.singleton (chr 0x00)

cobsEncode :: B.ByteString -> B.ByteString
cobsEncode
  = B.concat
  . map enc
  . chunks
  . B.take cobsEncodeMaxLength
  where
    enc :: B.ByteString -> B.ByteString
    enc chunk = chr (B.length chunk + 1) `B.cons` chunk
    chunks :: B.ByteString -> [B.ByteString]
    chunks = B.split '\0'

cobsDecode :: B.ByteString -> B.ByteString
cobsDecode
  = B.init
  . B.concat
  . map enc
  . chunks
  . B.take cobsDecodeMaxLength
  where
    enc :: B.ByteString -> B.ByteString
    enc chunk = B.tail chunk `B.snoc` chr 0x00
    chunks :: B.ByteString -> [B.ByteString]
    chunks = unfoldr $ \xs ->
      if B.null xs
      then Nothing
      else Just $ B.splitAt (ord $ B.head xs) xs
