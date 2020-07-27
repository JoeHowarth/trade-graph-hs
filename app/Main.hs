module Main where

import Lib 
import Types
import Load

import qualified GHC.Generics as G
import Data.Data
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
main = 
  do 
    (a,g) <- load
    putStrLn $ G.prettify g
    let (a', g') = run 1 (a,g)
    putStrLn $ G.prettify g'
  where
    run 0 s = s 
    run n s = run (n-1) $ loop s  


display :: ([Agent], Gr City ()) -> IO ()
display (agents, graph) = do
  pure ()
