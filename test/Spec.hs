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
  , priceTests
  ]

priceTests = testGroup "Test price functions"
  [ testCase "No profit made by buying and selling" $
      10 * pf1 50 @?= 10 * pf1 40
  , testCase "sellDelta simple" $
      sellDelta 2 mInfo1 @?= 0
  , testCase "Step price sold = difference" $ 
      stepPrice pf1 (sellDelta 2 mInfo1) mInfo1 @?= mInfo1
  , testCase "Step price over sold" $ 
      stepPrice pf1 (sellDelta 12 mInfo1) mInfo1 @?= mInfo1 {price = 70}
  , testCase "Step price under sold" $ 
      stepPrice pf1 (sellDelta 1 mInfo1) mInfo1 @?= mInfo1 {price = 48}

  ]
  where 
    pf1 = priceFunGen (50, 50, 2)
    mInfo1 = MarketInfo 20 50 18 (pf1 50)


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


mkAgent node good id = Agent (LCity $ CityLabel node) (Good good) id

agentStepTests = testGroup "Agent step related tests"
  [ testCase "bestTrade1" $ bestTrade mm1 mm2 @?= (2, Good "B")
  , testCase "bestTrade2" $ bestTrade mm2 mm1 @?= (3, Good "C")
  , testCase "bestTradeSelf" $ bestTrade mm1 mm1 @?= (0, Good "C")
  , testCase "stepAgent, 1 city" $ stepAgent (mkAgent 1 "A" 0) graph1 @?= mkAgent 2 "B" 0
  , testCase "stepAgent, 2 city" $ stepAgent (mkAgent 2 "A" 0) graph1 @?= mkAgent 1 "C" 0
  ]


exampleTests = testGroup "Example Unit Tests" 
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= GT
  ]
