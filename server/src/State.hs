module State
  ( State
  , (!?)
  , fromConfig
  , fromConfigThrow
  , update
  , notMember
  , prettyPrint
  ) where

import Data.IntMap.Strict
  ( IntMap
  , (!?)
  , update
  , notMember
  )
import qualified Data.IntMap.Strict as IntMap
import Data.List
  ( sortOn
  , groupBy
  )
import Control.Exception (throwIO)
import Data.Text (unpack)

import Types.Prop (Prop(..))
import Config.Prop (ConfigProp)
import qualified Config.Prop as CP
import Config (ConfigException(..))

type State = IntMap Prop

fromConfigThrow :: [ConfigProp] -> IO State
fromConfigThrow cProps = either throwIO return $ fromConfig cProps

fromConfig :: [ConfigProp] -> Either ConfigException State
fromConfig cProps
  | not . null $ addressConflicts = Left $ PropConflict "duplicate addresses" addressConflicts
  | not . null $ nameConflicts = Left $ PropConflict "duplicate names" nameConflicts
  | otherwise = Right $ IntMap.fromList (zip as ps)
  where
    addressConflicts = conflicts CP.address cProps
    nameConflicts = conflicts CP.name cProps
    ps = map fromConfigProp cProps
    as = map address ps

conflicts :: (Ord a) => (ConfigProp -> a) -> [ConfigProp] -> [[ConfigProp]]
conflicts f
  = filter ((> 1) . length)
  . groupBy (\x y -> f x == f y)
  . sortOn f

fromConfigProp :: ConfigProp -> Prop
fromConfigProp cProp = Prop
  { name = CP.name cProp
  , description = CP.description cProp
  , address = CP.address cProp
  , defaultValue = CP.defaultValue cProp
  , value = CP.defaultValue cProp
  }

prettyPrint :: State -> IO ()
prettyPrint = mapM_ f . IntMap.toList
  where
    f (addr, Prop{name, description, defaultValue, value}) = do
      let
        indent = replicate 2 ' '
        descString = case description of
          Just d -> " (" ++ unpack d ++ ")"
          Nothing -> ""
      putStrLn $ show addr ++ ": " ++ unpack name ++ descString
      putStrLn $ indent ++ "default value: " ++ show defaultValue
      putStrLn $ indent ++ "current value: " ++ show value
      putStrLn ""
