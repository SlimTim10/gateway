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
import Data.List (intercalate)

import Types.Rule.Trigger (Trigger)
import Types.Rule.Action (Action)

data Type = Basic | Sequence
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Type where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = map toLower }

type Description = Maybe Text

data Rule = Rule
  { type_ :: Type
  , description :: Description
  , trigger :: Trigger
  , action :: Action
  }
  deriving (Eq)

instance Show Rule where
  show Rule {type_, description, trigger, action}
    = intercalate ", "
      [ "type: " ++ show type_
      , maybe "" (\d -> "description: " ++ show d) description
      , "trigger: " ++ show trigger
      , "action: " ++ show action
      ]
    ++ "\n"
