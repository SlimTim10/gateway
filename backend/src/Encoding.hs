module Encoding
  -- (
  -- )
  where

import Data.ByteString.Char8 (ByteString)
import Data.List.Split (splitOn)

encode :: ByteString -> ByteString
encode = undefined

encodeCOBS :: [Int] -> [Int]
encodeCOBS = concat . map enc . chunks
  where
    chunks = splitOn [0x00]
    enc chunk = length chunk + 1 : chunk

encodeCOBSR :: [Int] -> [Int]
encodeCOBSR = encodeCOBSR' [] 0 []

encodeCOBSR' :: [Int] -> Int -> [Int] -> [Int] -> [Int]
encodeCOBSR' enc _ [] [] = enc
encodeCOBSR' enc chunkLength chunk [] = enc ++ chunk'
  where
    chunk' = chunkLength + 1 : chunk
encodeCOBSR' enc chunkLength chunk (0x00 : xs) = encodeCOBSR' enc' 0 [] xs
  where
    chunk' = chunkLength + 1 : chunk
    enc' = enc ++ chunk'
encodeCOBSR' enc chunkLength chunk (x : xs) = encodeCOBSR' enc (chunkLength + 1) (chunk ++ [x]) xs

-- encodeCOBSfold :: [Int] -> [Int]
-- encodeCOBSfold xs = foldr (f 0 []) [] xs
--   where
--     f 0x00 enc chunkLength chunk = enc ++ (chunkLength + 1 : chunk)
--     f x enc chunk =

encodeCOBSExplicit :: [Int] -> [Int]
encodeCOBSExplicit xs = go xs [] 0 []
  where
    go [] = \enc chunkLength chunk ->
      case chunk of
        [] -> enc
        ys -> enc ++ (chunkLength + 1 : chunk)
    go (x : xs) =
      \enc chunkLength chunk ->
        case x of
          0x00 ->
            let
              chunk' = chunkLength + 1 : chunk
              enc' = enc ++ chunk'
            in go xs enc' 0 []
          _ -> go xs enc (chunkLength + 1) (chunk ++ [x])

encodeCOBSFold :: [Int] -> [Int]
encodeCOBSFold xs = foldr f z xs [] 0 []
  where
    z enc _ [] = enc
    z enc chunkLength chunk = enc ++ (chunkLength + 1 : chunk)
    f 0x00 more enc chunkLength chunk = more enc' 0 []
      where
        chunk' = chunkLength + 1 : chunk
        enc' = enc ++ chunk'
    f x more enc chunkLength chunk = more enc (chunkLength + 1) (chunk ++ [x])
