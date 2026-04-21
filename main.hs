module Main where

import Game.Go

main = mapM (\i -> finishedGame' i `seq` return ()) [1..1000]
