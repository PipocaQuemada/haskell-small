module Game.TrivialGame where

import MonteCarlo.Game
import qualified Data.Map as M
import qualified Data.Set as S

instance Game Int where
  children x = [x..x+5] 
  simulateChild x = ( [x..x+3], even x )
