module Types 
  ( City(..), label, marketMap
  , MarketMap
  , MarketInfo(..), demand, supply, production, price
  , InputFormat(..)
  , EdgesInput(..)
  , AgentInput(..)
  , Good(..), unGood
  , CityLabel(..), unCityLabel
  , Agent(..), agentLoc, agentCargo, agentId, agentSrc , agentSrcNode
  , Loc(..)
  ) where

import qualified GHC.Generics as Gen
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Data
import Control.Lens
import qualified Data.Graph.Inductive as G

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