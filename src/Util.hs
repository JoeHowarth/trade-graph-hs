module Util 
  ( both'
  , extend
  ) where

import Data.Bifunctor (Bifunctor(bimap))
extend :: c -> (a,b) -> (a,b,c)
extend c (a,b) = (a,b,c)

both' :: (a -> b) -> (a,a) -> (b,b)
both' f = bimap f f 
