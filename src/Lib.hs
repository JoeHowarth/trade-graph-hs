module Lib
    ( readInput
    , makeGraph
    , InputFormat (..)
    , City (..)
    , MarketInfo (..)
    , Good (..)
    , CityLabel (..)
    , MarketMap
    ) where

import Data.Graph.Inductive.Graph (LNode, LEdge, mkGraph, prettify)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Bifunctor
import Data.Tuple
import GHC.Generics
import Data.Aeson
import Data.Maybe
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Control.Arrow ((>>>))
import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Map.Merge.Strict as M

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

newtype CityLabel = CityLabel { unCityLabel :: Int }
    deriving (Eq, Ord, Show, FromJSON)

data Agent = Agent { agentLoc :: Loc
                   , agentCargo :: Good
                   , agentId :: Int 
                   } deriving (Show)

data Loc = LCity CityLabel | LRoute CityLabel CityLabel
    deriving (Show)


loop :: ([Agent], Gr City ()) -> ([Agent], Gr City ())
loop (agents, graph) = (agents', graph')
    where
        graph' = stepPrices agents graph
        agents' = stepAgent <$> agents <*> pure graph'


stepPrices :: [Agent] -> Gr City () -> Gr City ()
stepPrices agents graph = stepCity `G.nmap` graph -- note done
    where 
        stepCity (City {marketMap}) = undefined


stepGood good m_info incoming leaving = undefined

-- 
stepAgent :: Agent -> Gr City () -> Agent
stepAgent (Agent {agentLoc = LCity label, agentCargo, agentId }) graph =  
    Agent dst good agentId
    where
        curNode = unCityLabel label
        ctx = G.context graph curNode :: G.Context City ()
        nbs = fromJust . G.lab graph <$> G.neighbors' ctx :: [City]
        -- dst = foldr (\(best, city) -> 
            -- if priceInCity city > priceInCity best then city else best) nbs
        dst = undefined
        good = undefined

priceInCity :: Good -> City -> Int
priceInCity good (City {marketMap}) = price $ marketMap M.! good

bestTrade :: MarketMap -> MarketMap -> (Int, Good)
bestTrade src dst = maximum . fmap swap . M.toList . merge src $ dst
    where 
        merge = M.merge M.dropMissing M.dropMissing (M.zipWithMatched zipper)
        zipper g src_info dst_info = price dst_info - price src_info



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



