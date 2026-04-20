module MonteCarlo.Game where

class Game a where
  children :: a -> [a]
  simulateChild :: a ->  -> Rand ([a], Bool)
