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

data Prop
  = Prop
    { name :: String
    , address :: Int
    , values :: [String]
    , state :: PropState
    }
  deriving (Show, Eq, Generic, ToJSON)

data PropState
  = PropState
    { value :: String
    , timestamp :: Integer
    }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Settings
  = Settings
    { address :: Int
    , values :: [String]
    , defaultValue :: String
    }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Settings where
  parseJSON = withObject "options" $ \o -> do
    address <- o .: "address"
    values <- (o .: "values" <|> (map showInt <$> o .: "values"))
    defaultValue <- (o .: "defaultValue" <|> (showInt <$> o .: "defaultValue"))
    if defaultValue `elem` values
      then return $ Settings address values defaultValue
      else fail "Default value is not in list of possible values"
    where
      showInt :: Int -> String
      showInt = show

instance FromJSON Prop where
  parseJSON = withObject "prop" $ \o -> do
    let [(name, options)] = HM.toList o
    let name' = T.unpack name
    options' <- parseJSON options
    let address' = (address :: Settings -> Int) options'
    let values' = (values :: Settings -> [String]) options'
    let defaultValue' = defaultValue options'
    return $ Prop name' address' values' (PropState defaultValue' 0)


-- type Timestamp = Integer
-- data PropState = PropState
--   { prop :: Prop
--   , timestamp :: Timestamp
--   }
--   deriving (Show)


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
