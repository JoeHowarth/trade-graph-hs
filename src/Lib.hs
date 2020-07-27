module Lib
  ( bestTrade
  , priceFunGen
  , stepAgent
  , stepPrice
  , stepPricesBuying
  , stepPricesSelling
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
loop (selling, graph) = (buying, graph'')
  where
    graph' = stepPricesSelling priceF (toMap selling) graph
    buying = stepAgent <$> selling <*> pure graph'
    graph'' = stepPricesBuying priceF (toMap buying) graph
    priceF = priceFunGen (100, 100, 1)
    toMap xs = M.fromList $ fmap (\a -> (a ^. agentLoc & loc2label, [a])) xs 
    loc2label (LCity loc) = loc
    loc2label (LRoute _ _) = undefined


stepPricesBuying
  :: (Int -> Int)
  -> Map CityLabel [Agent]
  -> Gr City () 
  -> Gr City ()
stepPricesBuying priceF buying graph = stepCity `G.nmap` graph -- not done
  where 
    stepCity city = city & marketMap %~ M.mapWithKey inner 
      where
        agents = buying ^.. at (city ^. label) . folded . folded . to (\a -> (a ^. agentCargo, [a]))
        good2Agent = M.fromListWith (++) agents
        numBuying good = lengthOf (at good . non []) good2Agent
        inner g m1 = stepPrice priceF (-(numBuying g)) m1
        
stepPricesSelling 
  :: (Int -> Int)
  -> Map CityLabel [Agent]
  -> Gr City () 
  -> Gr City ()
stepPricesSelling priceF selling graph = stepCity `G.nmap` graph -- not done
  where 
    stepCity city@(City {_marketMap, _label}) = city & marketMap %~ M.mapWithKey inner
      where
        agents = selling ^.. at (city ^. label) . folded . folded . to (\a -> (a ^. agentCargo, [a]))
        good2Agent = M.fromListWith (++) agents
        numBuying good = lengthOf (at good . non []) good2Agent
        inner g m1 = stepPrice priceF (numBuying g) m1

stepCity :: (Int -> Int) -> [Agent] -> City -> City
stepCity priceF selling city = undefined

sellDelta :: Int -> MarketInfo -> Int
sellDelta sold info = info^.production - info^.demand + sold

-- price_function factor sold, production, consumption
stepPrice :: (Int -> Int) -> Int -> MarketInfo -> MarketInfo
stepPrice priceFun supplyChange info = info & price .~ priceFun (info^.supply + supplyChange)

priceFunGen :: (Int,Int,Int) -> Int -> Int
priceFunGen (s0, p0, slope) amt = slope * (amt - s0) + p0

-- costToBuy :: Fractional a => (a -> a) -> a -> a
-- costToBuy priceF amt = amt * (priceF $ amt/2)

stepGood good m_info incoming leaving = undefined

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









