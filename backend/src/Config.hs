module Config
  -- (
  -- )
  where

import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  , (.:)
  , withObject
  , parseJSON
  )
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

data Prop
  = Prop
    { name :: String
    , address :: Int
    , defaultValue :: Value
    }
  deriving (Show, Eq, Generic, ToJSON)

data Value
  = Integer Int
  | IntegerList [Int]
  | String String
  deriving (Show, Eq, Generic, ToJSON)

parseSettings :: A.Value -> A.Parser (Int, Value)
parseSettings = withObject "settings" $ \o -> do
  address <- o .: "address"
  valueType <- o .: "value-type"
  defaultValue <- case valueType of
    "integer" -> do
      dv <- o .: "default-value"
      return $ Integer dv
    "integer-list" -> do
      dv <- o .: "default-value"
      return $ IntegerList dv
    "string" -> do
      dv <- o .: "default-value"
      return $ String dv
    _ -> fail $ "unknown value-type: " ++ valueType
  return (address, defaultValue)

-- decodeEither' "Tag Reader 1:\n  address: 1\n  value-type: integer\n  default-value: 1" :: Either ParseException Prop
-- decodeEither' "Door:\n  address: 3\n  value-type: string\n  default-value: \"Closed\"" :: Either ParseException Prop
-- decodeEither' "Tag Reader 1:\n  address: 1\n  value-type: integer-list\n  default-value: [0,0,0]" :: Either ParseException Prop
instance FromJSON Prop where
  parseJSON = withObject "prop" $ \o -> do
    let [(name', settings)] = HM.toList o
    let name = T.unpack name'
    (address, defaultValue) <- parseSettings settings
    return $ Prop name address defaultValue

-- data Config = Config
--   { props :: [Prop]
--   , rules :: [Rule]
--   }

-- data Rule
--   = RuleBasic [Prop] Prop
--   | RuleSequence [Prop] Prop
--   | RuleTimedSequence [Prop] [Int] Prop

-- Config
--   { props =
--     [ 
--     ]
--   , triggers =
--     [ RuleBasic [TagReader1 1] (Door Open)
--     , RuleBasic [TagReader1 3, TagReader2 2] (Door Open)
--     , RuleSequence [TagReader1 1, TagReader2 2] (Door Close)
--     , RuleTimedSequence [TagReader1 2, TagReader2 3] [0, 1] (Door Open)
--     ]
--   }
