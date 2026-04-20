module Game.TicTacToe where

import MonteCarlo.Game
import Data.Set as S
import MonteCarlo.TreeSearch

data Player = X | O

otherPlayer X = O
otherPlayer O = X

type Row = (Maybe Player, Maybe Player, Maybe Player)
data Board = Board Player Row Row Row 

instance Game Board where
    children = undefined 
    simulateChild a = ( [a], True)
