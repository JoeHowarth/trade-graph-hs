module Lib
  ( readInput
  , makeGraph
  , bestTrade
  , priceFunGen
  , stepAgent
  , stepPrice
  , stepPricesBuying
  , stepPricesSelling
  , sellDelta
  , loop
  , InputFormat (..)
  , City (..)
  , label, marketMap
  , MarketInfo (..)
  , demand, supply, production, price
  , Good (..)
  , unGood
  , CityLabel (..)
  , unCityLabel
  , MarketMap
  , Agent (..)
  , agentCargo, agentLoc, agentId
  , Loc (..)
  ) where

import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.Graph (LNode, LEdge, mkGraph, prettify)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Basic (undir)
import Data.Bifunctor
import Data.Function (on)
import Data.Tuple
import qualified GHC.Generics as Gen
import Data.List (maximumBy)
import Data.Data
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import Control.Lens
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import Data.Map.Strict (Map)
import Control.Arrow ((>>>), (&&&))
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Text.PrettyPrint.Tabulate as T

data City = City { _label :: CityLabel
                 , _marketMap :: MarketMap 
                 } deriving (Show, Gen.Generic, Data)

type MarketMap = Map Good MarketInfo

data MarketInfo = MarketInfo { _demand :: Int 
                             , _supply :: Int 
                             , _production :: Int 
                             , _price :: Int 
                             } deriving (Show, Data, Gen.Generic, Eq, Ord)

data InputFormat = InputFormat { cities :: [City] 
                               , edgeInput :: EdgesInput 
                               , agentInput :: AgentInput 
                               } deriving (Show, Data, Gen.Generic)
instance FromJSON InputFormat

type EdgesInput = [(CityLabel, CityLabel)]
type AgentInput = [(CityLabel, Int)]
         
newtype Good = Good { _unGood :: String }
  deriving (Eq, Ord, Show, ToJSONKey, FromJSONKey, Gen.Generic, Data)

newtype CityLabel = CityLabel { _unCityLabel :: Int }
  deriving (Eq, Ord, Show, Data, Gen.Generic)

instance T.CellValueFormatter MarketMap

data Agent = 
     Agent { _agentLoc :: Loc
           , _agentCargo :: Good
           , _agentId :: Int 
           } deriving (Show, Eq, Ord, Gen.Generic, Data)

data Loc = LCity CityLabel | LRoute CityLabel CityLabel
  deriving (Show, Eq, Ord, Gen.Generic, Data)

concat <$> mapM (deriveJSON defaultOptions{fieldLabelModifier = drop 1, unwrapUnaryRecords = True}) [''City, ''CityLabel, ''Agent, ''Good, ''MarketInfo, ''Loc]
concat <$> mapM makeLenses [''City, ''Agent, ''Good, ''MarketInfo, ''CityLabel]

agentSrc :: Lens' Agent CityLabel
agentSrc = lens getter setter
  where 
    getter a = case _agentLoc a of 
      LCity x -> x 
      LRoute x _ -> x
    setter a v = a & agentLoc %~ inner v
    inner v (LCity _) = LCity v
    inner v (LRoute _ y) = LRoute v y

agentSrcNode :: Lens' Agent G.Node
agentSrcNode = agentSrc . unCityLabel

loop :: ([Agent], Gr City ()) -> ([Agent], Gr City ())
loop (selling, graph) = (buying, graph'')
  where
    graph' = stepPricesSelling sellDelta priceF (toMap selling) graph
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
  :: (Int -> MarketInfo -> Int) 
  -> (Int -> Int)
  -> Map CityLabel [Agent]
  -> Gr City () 
  -> Gr City ()
stepPricesSelling deltaF priceF selling graph = stepCity `G.nmap` graph -- not done
  where 
    stepCity city@(City {_marketMap, _label}) = city & marketMap %~ M.mapWithKey inner
      where
        agents = selling ^.. at (city ^. label) . folded . folded . to (\a -> (a ^. agentCargo, [a]))
        good2Agent = M.fromListWith (++) agents
        numBuying good = lengthOf (at good . non []) good2Agent
        inner g m1 = stepPrice priceF (numBuying g) m1



sellDelta :: Int -> MarketInfo -> Int
sellDelta sold info = info^.production - info^.demand + sold

-- price_function factor sold, production, consumption
stepPrice :: (Int -> Int) -> Int -> MarketInfo -> MarketInfo
stepPrice priceFun delta info = info & price .~ priceFun (info^.supply + delta)

priceFunGen :: (Int,Int,Int) -> Int -> Int
priceFunGen (s0, p0, slope) amt = slope * (amt - s0) + p0

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



readInput :: IO (Either String InputFormat)
readInput = B.readFile "input.json" >>= pure . eitherDecode 
  
-- type Network = Gr City ()

makeGraph :: [City] -> EdgesInput -> Gr City ()
makeGraph cities edgesInput = undir $ mkGraph nodes edges
  where
    -- nodes = (\c -> (c ^. label . unCityLabel , c)) <$> cities
    nodes = cities ^.. each . to (view (label . unCityLabel) &&& id) :: [(Int, City)]
    labelMap = M.fromList $ (over _1 (view label)) . swap <$> nodes :: Map CityLabel Int
    -- edges = extend () . both' ((M.!) labelMap) <$> edgesInput
    edges = extend () . (over each ((M.!) labelMap)) <$> edgesInput


both' :: (a -> b) -> (a,a) -> (b,b)
both' f = bimap f f 

extend :: c -> (a,b) -> (a,b,c)
extend c (a,b) = (a,b,c)



