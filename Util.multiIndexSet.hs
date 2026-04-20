{-# Language TemplateHaskell #-}

module MultiIndexSet where

import Data.Set as S
import Data.Map as M
import Control.Lens

{- Provides something similar to a set, but multiple values *share* 
 - a single value.  If x and y share a value, than 'modify multiIndexSet x f'
 - also modifies the value associated with y.
 -
 - This is useful for e.g. memoizing connected component information on a graph
 - you're constantly adding to - you can add yourself to all n items you're connected to in O(1) time.
 -}

data MultiIndexSet k v = MultiIn
