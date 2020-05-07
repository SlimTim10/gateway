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
import Data.Word (Word8)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.FastBuilder
  ( toStrictByteString
  , word8
  )
import Data.List (intercalate)

type Name = Text
type Description = Maybe Text
type Address = Int

data Value
  = Int Word8
  | IntList [Word8]
  | String String
  | Nothing
  deriving (Eq, Generic, ToJSON)

instance Show Value where
  show (Int x) = show x
  show (IntList x) = show x
  show (String x) = x
  show Nothing = "Nothing"

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
  deriving (Eq)

instance Show Prop where
  show Prop {name, description, address, defaultValue, value}
    = intercalate ", "
      [ "name: " ++ show name
      , maybe "" (\d -> "description: " ++ show d) description
      , "address: " ++ show address
      , "defaultValue: " ++ show defaultValue
      , "value: " ++ show value
      ]
    ++ "\n"

rawValue :: Value -> B.ByteString
rawValue Nothing = B.empty
rawValue (Int x) = toStrictByteString . word8 $ x
rawValue (IntList xs) = mconcat . map (toStrictByteString . word8) $ xs
rawValue (String s) = B.snoc (B.pack s) '\0'
