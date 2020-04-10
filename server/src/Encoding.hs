module Encoding
  ( encodeCOBS
  , decodeCOBS
  , encodeCOBSMaxLength
  , decodeCOBSMaxLength
  , cobsBoundary
  )
  where

import qualified Data.ByteString.Char8 as B
import Data.Char (chr, ord)
import Data.List (unfoldr)

encodeCOBSMaxLength :: Int
encodeCOBSMaxLength = 254

decodeCOBSMaxLength :: Int
decodeCOBSMaxLength = 255

cobsBoundary :: B.ByteString
cobsBoundary = B.singleton (chr 0x00)

encodeCOBS :: B.ByteString -> B.ByteString
encodeCOBS
  = B.concat
  . map enc
  . chunks
  . B.take encodeCOBSMaxLength
  where
    enc :: B.ByteString -> B.ByteString
    enc chunk = chr (B.length chunk + 1) `B.cons` chunk
    chunks :: B.ByteString -> [B.ByteString]
    chunks = B.split '\0'

decodeCOBS :: B.ByteString -> B.ByteString
decodeCOBS
  = B.init
  . B.concat
  . map enc
  . chunks
  . B.take decodeCOBSMaxLength
  where
    enc :: B.ByteString -> B.ByteString
    enc chunk = B.tail chunk `B.snoc` chr 0x00
    chunks :: B.ByteString -> [B.ByteString]
    chunks = unfoldr $ \xs ->
      if B.null xs
      then Nothing
      else Just $ B.splitAt (ord $ B.head xs) xs
