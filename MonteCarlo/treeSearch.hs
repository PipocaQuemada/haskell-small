{-# LANGUAGE TupleSections #-}

module MonteCarlo.TreeSearch where

import Prelude hiding (sequence)

import Control.Comonad
import Control.Comonad.Cofree

import Data.IORef
import System.IO.Unsafe

import Control.Arrow
import Control.Monad hiding (sequence)
import Data.Traversable
import Data.Monoid
import Data.Ord
import Data.Ratio
import Control.Lens hiding (children)
import Data.List 

import MonteCarlo.Game

import qualified Data.Map as M

data Winrate = W Integer Integer deriving (Show, Eq) -- W wins total

instance Monoid Winrate where
  mappend (W wins total) (W wins2 total2) = W (wins + wins2) (total + total2)
  mempty = W 0 0

class Score a where
  singleObs :: Bool -> a

instance Score Winrate where
  singleObs True = W 1 1
  singleObs False = W 0 1

winrateToFractional (W x 0) = 0
winrateToFractional (W wins total) =  fromInteger wins / fromInteger total

instance Ord Winrate where
  compare = comparing winrateToFractional

-- x n nj where x = average winrate from this arm
--              n = number of times parent node was visited
--              nj = number of times this node was visited
data UCT = UCT Winrate Integer Integer deriving (Eq, Show) -- x n nj

instance Monoid UCT where
  mappend (UCT (W win total) n nj) (UCT (W win2 total2) n2 nj2) = UCT (W (win + win2) (total + total2) ) (n + n2) (nj + nj2)
  mempty = UCT mempty 0 0

uctToFractional (UCT winrate parentTimes nodeTimes) = w + sqrt (log n / (5*nj) ) 
   where w = winrateToFractional winrate
         n = fromInteger parentTimes
         nj = fromInteger nodeTimes


instance Ord UCT where
  compare = comparing $ \(UCT x n nj) -> winrateToFractional x + sqrt (2 * log (fromInteger n) )

type GameTree score game = Cofree [] (game, score)

getGame = fst . extract

-- combine game trees containing different simulations
-- useful for generating one game tree per core, then combining the results.
combineTrees gt@( (game, score) :< children) ( (game', score') :< children') =
  if (game == game') 
    then (game, score <> score') :< combineChildren children children'
    else gt -- can't combine *different* game trees
  where combineChildren [] xs = xs
        combineChildren xs [] = xs
        combineChildren xs ys = zipWith combineTrees xs ys

unvisited g = (g, mempty) :< [] 

visit node@((g,score) :< _) = if score == mempty
                              then visitInitially g
                              else revisit node

visitInitially g = (res, (g, res) :< (map unvisited $ children g))
   where res = singleObs . snd $ simulateChild g

revisit ((g,s) :< children) = (res, (g, s <> res) :< children')
  where bestChild = maximumBy (comparing $ snd . extract) children
        (res , newChild) = visit bestChild
        children' = newChild : deleteBy (\x y -> getGame x == getGame y) bestChild children

