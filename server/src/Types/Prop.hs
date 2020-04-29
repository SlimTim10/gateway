module Types.Prop where

import Prelude hiding (Nothing)
import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  , parseJSON
  )
import Data.Aeson
  ( genericParseJSON
  , defaultOptions
  , Options(..)
  , SumEncoding(..)
  )
import Data.Word (Word8, Word32)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.ByteString.Builder
  ( toLazyByteString
  , word8
  )

type Name = Text
type Description = Maybe Text
type Address = Word32

data Value
  = Int Word8
  | IntList [Word8]
  | String String
  | Nothing
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Value where
  parseJSON = genericParseJSON
    defaultOptions { sumEncoding = UntaggedValue }

data Prop = Prop
  { name :: Name
  , description :: Description
  , address :: Address
  , defaultValue :: Value
  , value :: Value
  }
  deriving (Show, Eq)

rawValue :: Value -> B.ByteString
rawValue Nothing = B.empty
rawValue (Int x) = toStrict . toLazyByteString . word8 $ x
rawValue (IntList xs) = toStrict . mconcat . map (toLazyByteString . word8) $ xs
rawValue (String s) = B.pack s
