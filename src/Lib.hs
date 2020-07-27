module Lib
  ( bestTrade
  , priceFunGen
  , stepAgent
  , stepPrice
  , stepCity
  , sellDelta
  , loop
  -- , InputFormat (..) -- , City (..) -- , label, marketMap -- , MarketInfo (..) -- , demand, supply, production, price -- , Good (..) -- , unGood -- , CityLabel (..) -- , unCityLabel -- , MarketMap -- , Agent (..) -- , agentCargo, agentLoc, agentId -- , Loc (..)
  ) where

import Types
import Util

import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.Graph (LNode, LEdge, mkGraph, prettify)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Basic (undir)
import Data.Bifunctor
import Data.Function (on)
import Data.Tuple
import Data.Maybe
import qualified GHC.Generics as Gen
import Data.List (maximumBy)
import Control.Lens
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import Data.Map.Strict (Map)
import Control.Arrow ((>>>), (&&&))
import Data.Graph.Inductive.PatriciaTree (Gr)


loop :: ([Agent], Gr City ()) -> ([Agent], Gr City ())
loop (selling, graph) = (buying, nextGraph)
  where
    graphAfterSelling = stepPrices (stepPrice priceF) (toMap selling) graph
    buying = stepAgent <$> selling <*> pure graphAfterSelling
    nextGraph = stepPrices (\bought -> stepPrice priceF (-bought)) (toMap buying) graphAfterSelling
    --
    priceF = priceFunGen (100, 100, 1)
    toMap xs = M.fromList $ ((loc2label . view agentLoc) &&& pure) <$> xs :: Map CityLabel [Agent]

stepPrices :: (Int -> MarketInfo -> MarketInfo) 
  -> Map CityLabel [Agent]
  -> Gr City ()
  -> Gr City ()
stepPrices stepPriceF agentMap = G.nmap (\city -> stepCity stepPriceF (agentsInCity city) city) 
  where 
    agentsInCity city = fromMaybe [] $ M.lookup (city^.label) agentMap 

    
stepCity :: (Int -> MarketInfo -> MarketInfo) 
  -> [Agent] 
  -> City 
  -> City
stepCity stepPriceF agentsInCity city = city & marketMap %~ M.mapWithKey stepGood
  where
    good2Agent = M.fromListWith (++) $ (view agentCargo &&& pure) <$> agentsInCity :: Map Good [Agent]
    numSelling good = fromMaybe 0 $ length <$> M.lookup good good2Agent 
    stepGood good mm = stepPriceF (numSelling good) mm

sellDelta :: Int -> MarketInfo -> Int
sellDelta sold info = info^.production - info^.demand + sold

-- price_function factor sold, production, consumption
stepPrice :: (Int -> Int) -> Int -> MarketInfo -> MarketInfo
stepPrice priceFun supplyChange info = info & price .~ priceFun (info^.supply + supplyChange)

priceFunGen :: (Int,Int,Int) -> Int -> Int
priceFunGen (s0, p0, slope) amt = slope * (amt - s0) + p0

-- costToBuy :: Fractional a => (a -> a) -> a -> a
-- costToBuy priceF amt = amt * (priceF $ amt/2)

stepAgent :: Agent -> Gr City () -> Agent
stepAgent agent g = agent & agentCargo .~ good & agentLoc .~ LCity dst
  where
    curNode = agent ^. agentSrcNode :: G.Node
    ctx = G.context g curNode :: G.Context City ()
    nbs = fromJust . G.lab g <$> G.neighbors' ctx :: [City]
    bestTradeAp = bestTrade . _marketMap . fromJust $ G.lab g curNode
    (profit, good, dst) = maximum $ (\x -> extend (x^.label) . bestTradeAp $ _marketMap x) <$> nbs

priceInCity :: Good -> Lens' City Int
priceInCity good = marketMap . at good . non zeros . price
  where zeros = MarketInfo 0 0 0 0

-- Find the most profitable good to buy in src and sell in dst 
bestTrade :: MarketMap -> MarketMap -> (Int, Good)
bestTrade src dst = maximum . fmap swap . M.toList . merge src $ dst
  where 
    merge = M.merge M.dropMissing M.dropMissing (M.zipWithMatched zipper)
    zipper g src_info dst_info = dst_info^.price - src_info^.price









