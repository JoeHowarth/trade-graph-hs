module Lib
  ( readInput
  , makeGraph
  , bestTrade
  , priceFunGen
  , stepAgent
  , stepPrice
  , stepPrices
  , sellDelta
  , InputFormat (..)
  , City (..)
  , MarketInfo (..)
  , Good (..)
  , CityLabel (..)
  , MarketMap
  , Agent (..)
  , Loc (..)
  ) where

import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.Graph (LNode, LEdge, mkGraph, prettify)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Basic (undir)
import Data.Bifunctor
import Data.Function (on)
import Data.Tuple
import GHC.Generics
import Data.List (maximumBy)
import Data.Aeson
import Data.Maybe
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import Data.Map.Strict (Map)
import Control.Arrow ((>>>))
import Data.Graph.Inductive.PatriciaTree (Gr)

data City = City { label :: CityLabel
                 , marketMap :: MarketMap 
                 } deriving (Show, Generic)
instance FromJSON City

type MarketMap = Map Good MarketInfo

data MarketInfo = MarketInfo { demand :: Int 
                             , supply :: Int 
                             , production :: Int 
                             , price :: Int 
                             } deriving (Show, Generic, Eq, Ord)
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
           } deriving (Show, Eq, Ord)

data Loc = LCity CityLabel | LRoute CityLabel CityLabel
  deriving (Show, Eq, Ord)


loop :: ([Agent], Gr City ()) -> ([Agent], Gr City ())
loop (agents, graph) = (agents', graph')
  where
    agents' = stepAgent <$> agents <*> pure graph
    graph' = stepPrices agents graph

stepPrices :: [Agent] -> Gr City () -> Gr City ()
stepPrices selling graph = stepCity `G.nmap` graph -- not done
  where 
    stepCity (City {marketMap}) = undefined
    x as = (\a -> (loc2label $ agentLoc a, agentCargo a)) <$> as 
    loc2label (LCity label) = label
    loc2label (LRoute _ _) = undefined


sellDelta :: Int -> MarketInfo -> Int
sellDelta sold info = production info - demand info + sold

-- price_function factor sold, production, consumption
stepPrice :: (Int -> Int) -> Int -> MarketInfo -> MarketInfo
stepPrice priceFun delta info = info {price = price'}
  where 
    -- delta = 
    price' = priceFun $ supply info + delta

priceFunGen :: (Int,Int,Int) -> Int -> Int
priceFunGen (s0, p0, slope) amt = slope * (amt - s0) + p0



stepGood good m_info incoming leaving = undefined

stepAgent :: Agent -> Gr City () -> Agent
stepAgent (Agent {agentLoc = LCity name, agentCargo, agentId }) graph =  
  Agent (LCity dst) good agentId
  where
    curNode = unCityLabel name
    ctx = G.context graph curNode :: G.Context City ()
    nbs = fromJust . G.lab graph <$> G.neighbors' ctx :: [City]
    bestTradeAp = bestTrade . marketMap . fromJust $ G.lab graph curNode
    (profit, good, dst) = maximum $ (\x -> extend (label x) . bestTradeAp $ marketMap x) <$> nbs

priceInCity :: Good -> City -> Int
priceInCity good (City {marketMap}) = price $ marketMap M.! good

-- Find the most profitable good to buy in src and sell in dst 
bestTrade :: MarketMap -> MarketMap -> (Int, Good)
bestTrade src dst = maximum . fmap swap . M.toList . merge src $ dst
  where 
    merge = M.merge M.dropMissing M.dropMissing (M.zipWithMatched zipper)
    zipper g src_info dst_info = price dst_info - price src_info



readInput :: IO (Either String InputFormat)
readInput = B.readFile "input.json" >>= pure . eitherDecode 
  
-- type Network = Gr City ()

makeGraph :: [City] -> EdgesInput -> Gr City ()
makeGraph cities edgesInput = undir $ mkGraph nodes edges
  where
    nodes = (\c -> (unCityLabel $ label c, c)) <$> cities
    labelMap = M.fromList $ first label . swap <$> nodes
    edges = extend () . both ((M.!) labelMap) <$> edgesInput

both :: (a -> b) -> (a,a) -> (b,b)
both f = bimap f f 

extend :: c -> (a,b) -> (a,b,c)
extend c (a,b) = (a,b,c)



