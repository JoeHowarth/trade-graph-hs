module Main where

import Lib

import Data.Graph.Inductive.Graph (prettify)
import Control.Arrow ((>>>))

main :: IO ()
main = do 
    input <- readInput 
    case input of 
        Left err -> fail err
        Right (Lib.InputFormat {cities, edgeInput, agentInput}) -> do
            let g = makeGraph cities edgeInput
            putStrLn $ prettify g



f :: [Cities] -> EdgeInput -> String
f cities edgeInput = (prettify . makeGraph ) cities edgeInput

    



