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
import Control.Applicative
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


-- repeat :: Monad m => (a -> m a) -> a -> [m a]
-- repeat f a = go
--   where 
--     go f a as =  

repeatApp :: a -> (a -> a) -> [a]
repeatApp x f = y : repeatApp y f
  where y = f x

repeatAppM :: Monad m => a -> (a -> m a) -> [m a]
repeatAppM x f = do
  y <- f x 
  (pure y ) : (repeatApp y f)

replicateM :: (Applicative m) => Int -> m a -> m [a]
replicateM cnt0 f =
    loop cnt0
  where
    loop cnt
        | cnt <= 0  = pure []
        -- | otherwise = liftA2 (:) f (loop (cnt - 1))
        | otherwise = (:) <$> f <*> (loop (cnt - 1))