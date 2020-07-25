module Main where

import Lib 

import Text.PrettyPrint.Tabulate
import qualified GHC.Generics as G
import Data.Data
import qualified Text.PrettyPrint.Tabulate as T
import Control.Arrow ((>>>))
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


main :: IO ()
main = do 
  input <- readInput 
  case input of 
    Left err -> fail err
    Right (InputFormat{cities, edgeInput, agentInput}) -> do
      let g = makeGraph cities edgeInput
          a = makeAgents cities agentInput
      putStrLn $ G.prettify g
      let (a', g') = run 1 (a,g)
      putStrLn $ G.prettify g'
      pure ()
  where
    run 0 s = s 
    run n s = run (n-1) $ loop s  


makeAgents :: [City] -> [(CityLabel, Int)] -> [Agent]
makeAgents cities input = agents ^. _1
  where 
    good = head cities ^. marketMap . to M.findMin . _1 -- get a random good
    agents = foldr (\(l, num) (as, cnt) -> ([Agent (LCity l) good (cnt + i) | i <- [0..num-1]], cnt + num)) ([], 0) input

display :: ([Agent], Gr City ()) -> IO ()
display (agents, graph) = do
  pure ()
