module Main where

import Lib 

import Control.Arrow ((>>>))
import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Bifunctor
import Data.Tuple
import GHC.Generics
import Data.Aeson
import Data.Maybe
import System.IO
import Data.Map.Strict (Map)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M


main :: IO ()
main = do 
    input <- readInput 
    case input of 
        Left err -> fail err
        Right (Lib.InputFormat {cities, edgeInput, agentInput}) -> do
            let g = makeGraph cities edgeInput
            putStrLn $ G.prettify g

