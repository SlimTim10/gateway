module Types.Rule where

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
  )
import Data.Text (Text)
import Data.Char (toLower)

import qualified Types.Prop as Prop

data Type = Basic | Sequence
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Type where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = map toLower }

type Description = Maybe Text

data TriggerElement = TriggerElement
  { propKey :: Int
  , value :: Prop.Value
  }
  deriving (Show, Eq)

data ActionElement = ActionElement
  { propKey :: Int
  , value :: Prop.Value
  }
  deriving (Show, Eq)

type Trigger = [TriggerElement]
type Action = [ActionElement]

data Rule = Rule
  { type_ :: Type
  , description :: Description
  , trigger :: Trigger
  , action :: Action
  }
  deriving (Show, Eq)
