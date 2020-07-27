module Load 
  ( makeGraph
  , makeAgents
  , load
  ) where
import Types
import Util

import qualified GHC.Generics as G
import Data.Data
import Control.Arrow ((&&&), (>>>))
import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Bifunctor
import Data.Tuple
import Data.Aeson
import Data.Maybe
import System.IO
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import Data.Graph.Inductive (undir)

readInput :: IO (Either String InputFormat)
readInput = B.readFile "input.json" >>= pure . eitherDecode 
  
makeGraph :: [City] -> EdgesInput -> Gr City ()
makeGraph cities edgesInput = undir $ G.mkGraph nodes edges
  where
    -- nodes = (\c -> (c ^. label . unCityLabel , c)) <$> cities
    nodes = cities ^.. each . to (view (label . unCityLabel) &&& id) :: [(Int, City)]
    labelMap = M.fromList $ (over _1 (view label)) . swap <$> nodes :: Map CityLabel Int
    -- edges = extend () . both' ((M.!) labelMap) <$> edgesInput
    edges = extend () . (over each ((M.!) labelMap)) <$> edgesInput

makeAgents :: [City] -> [(CityLabel, Int)] -> [Agent]
makeAgents cities input = agents ^. _1
  where 
    good = head cities ^. marketMap . to M.findMin . _1 -- get a random good
    agents = foldr (\(l, num) (as, cnt) -> ([Agent (LCity l) good (cnt + i) | i <- [0..num-1]], cnt + num)) ([], 0) input


load :: IO ([Agent], Gr City ())
load = do
  input <- readInput 
  case input of 
    Left err -> fail err
    Right input -> do
      pure load input

make :: InputFormat -> ([Agent], Gr City ())
make (InputFormat{cities, edgeInput, agentInput}) = (a,g)
      where g = makeGraph cities edgeInput
            a = makeAgents cities agentInput