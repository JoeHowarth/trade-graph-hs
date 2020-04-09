{-# LANGUAGE PostfixOperators #-}
module Lib
    ( someFunc
    ) where

import Data.Graph.Inductive.Graph (mkGraph, prettify)
import Data.Graph.Inductive.PatriciaTree (Gr)
import GHC.Generics
import Data.Aeson
import System.IO
import Data.Map (Map)
import qualified Data.Map as M

data City = City { label :: CityLabel
                 , marketMap :: MarketMap 
                 } deriving (Show, Generic)
instance FromJSON City

type MarketMap = Map Good MarketInfo

data MarketInfo = MarketInfo { demand :: Int 
                             , supply :: Int
                             , production :: Int
                             , price :: Int 
                             } deriving (Show, Generic)
instance FromJSON MarketInfo
               
newtype Good = Good { unGood :: String }
    deriving (Eq, Ord, Show, Generic)
instance FromJSON Good
instance FromJSONKey Good

newtype CityLabel = CityLabel { unCityLabel :: String }
    deriving (Show, Generic)
instance FromJSON CityLabel 


someFunc :: IO ()
someFunc = putStrLn "someFunc"

