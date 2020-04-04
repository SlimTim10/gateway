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
import Control.Applicative ((<|>))
import Data.Scientific (floatingOrInteger)
import Data.Maybe (fromJust)

data Prop
  = Prop
    { name :: String
    , address :: Int
    , defaultValue :: Value
    }
  deriving (Show, Eq, Generic, ToJSON)

data Value
  = ValueInteger Int
  | ValueIntegerList [Int]
  | ValueString String
  deriving (Show, Eq, Generic, ToJSON)

parseValue :: A.Value -> A.Parser Value
parseValue = withObject "value" $ \o ->
  case HM.lookup "default-value" o of
    Just (A.Number x) -> case floatingOrInteger x of
      Left f -> fail "expected an integer"
      Right n -> return $ ValueInteger n
    Just (A.String x) -> return $ ValueString (T.unpack x)
    -- Just (A.Array xs) -> return _
    Nothing -> fail "expected an integer or string"

parseSettings :: A.Value -> A.Parser (Int, Value)
parseSettings = withObject "settings" $ \o -> do
  address <- o .: "address"
  defaultValue <- parseValue (A.Object o)
  return (address, defaultValue)

-- decodeEither' "Tag Reader 1:\n  address: 1\n  default-value: 1" :: Either ParseException Prop
-- decodeEither' "Door:\n  address: 3\n  default-value: \"Closed\"" :: Either ParseException Prop
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
