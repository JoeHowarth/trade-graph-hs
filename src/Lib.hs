module Lib
    ( readInput
    , makeGraph
    , InputFormat (..)
    , City
    , MarketInfo
    , Good
    , CityLabel
    ) where

import Data.Graph.Inductive.Graph (LNode, LEdge, mkGraph, prettify)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Bifunctor
import Data.Tuple
import GHC.Generics
import Data.Aeson
import Data.Maybe
import System.IO
import Data.Map (Map)
import qualified Data.ByteString.Lazy as B
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

data InputFormat = InputFormat { cities :: [City]
                               , edgeInput :: EdgesInput
                               , agentInput :: AgentInput
                               } deriving (Show, Generic)
instance FromJSON InputFormat

type EdgesInput = [(CityLabel, CityLabel)]
type AgentInput = [(CityLabel, Int)]
               
newtype Good = Good { unGood :: String }
    deriving (Eq, Ord, Show, FromJSON, FromJSONKey)

newtype CityLabel = CityLabel { unCityLabel :: String }
    deriving (Eq, Ord, Show, FromJSON)


readInput :: IO (Either String InputFormat)
readInput = B.readFile "input.json" >>= pure . eitherDecode 
    
-- type Network = Gr City ()

makeGraph :: [City] -> EdgesInput -> Gr City ()
makeGraph cities edgesInput = mkGraph nodes edges
    where
        nodes = zip [0..length cities - 1] cities
        labelMap = M.fromList $ first label . swap <$> nodes
        edges = extend () . both ((M.!) labelMap) <$> edgesInput

both :: (a -> b) -> (a,a) -> (b,b)
both f = bimap f f 

extend :: c -> (a,b) -> (a,b,c)
extend c (a,b) = (a,b,c)




