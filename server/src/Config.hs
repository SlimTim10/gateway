module Config where

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

import qualified Prop

data ConfigProp
  = ConfigProp
    { name :: String
    , address :: Int
    , defaultValue :: Prop.Value
    }
  deriving (Show, Eq, Generic, ToJSON)

parseSettings :: A.Value -> A.Parser (Int, Prop.Value)
parseSettings = withObject "settings" $ \o -> do
  address <- o .: "address"
  valueType <- o .: "value-type"
  defaultValue <- case valueType of
    "integer" -> do
      dv <- o .: "default-value"
      return $ Prop.Int dv
    "integer-list" -> do
      dv <- o .: "default-value"
      return $ Prop.IntList dv
    "string" -> do
      dv <- o .: "default-value"
      return $ Prop.String dv
    _ -> fail $ "unknown value-type: " ++ valueType
  return (address, defaultValue)

instance FromJSON ConfigProp where
  parseJSON = withObject "prop" $ \o -> do
    let [(name', settings)] = HM.toList o
    let name = T.unpack name'
    (address, defaultValue) <- parseSettings settings
    return $ ConfigProp name address defaultValue

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
