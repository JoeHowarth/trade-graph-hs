import Lib

import Test.Tasty
import Test.Tasty.HUnit
import Data.Bifunctor
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import qualified Data.Set as S
import Data.Map.Strict (Map)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [ agentStepTests
  , makeGraphTests
  ]

makeGraphTests = testGroup "Test making graph"
  [ testCase "simple graph undirected" $ 
      G.ufold ((&&) . inEqualOut) True graph1 
        @?= True
  , testCase "node matches label" $
      G.ufold (\(_, n, City {label}, _) b -> b && unCityLabel label == n) True graph1
        @?= True
  ]
  where 
    inEqualOut ctx = (S.fromList . fmap (\(a,b,_) -> (b,a)) . G.out') ctx 
      == (S.fromList . fmap (\(a,b,_) -> (a,b)) . G.inn') ctx 

mmFromGPpairs :: [(String, Int)] -> MarketMap
mmFromGPpairs xs = M.fromList $ bimap Good (\p -> base {price = p}) <$> xs
  where 
    base = MarketInfo 0 0 0 0 

mm1 = mmFromGPpairs [("A", 5), ("B", 4), ("C", 7)]
mm2 = mmFromGPpairs [("A", 3), ("B", 6), ("C", 4)]
city1 = City (CityLabel 1) mm1
city2 = City (CityLabel 2) mm2
graph1 = makeGraph [city1, city2] [(CityLabel 1, CityLabel 2)]
agent = mkAgent 1 "A" 0

mkAgent node good id = Agent (LCity $ CityLabel node) (Good good) id

agentStepTests = testGroup "Agent step related tests"
  [ testCase "bestTrade1" $ bestTrade mm1 mm2 @?= (2, Good "B")
  , testCase "bestTrade2" $ bestTrade mm2 mm1 @?= (3, Good "C")
  , testCase "bestTradeSelf" $ bestTrade mm1 mm1 @?= (0, Good "C")
  , testCase "stepAgent, 1 city" $ stepAgent agent graph1 @?= mkAgent 2 "B" 0
  ]


exampleTests = testGroup "Example Unit Tests" 
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= GT
  ]
