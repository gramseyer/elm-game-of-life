module GameOfLife where

import Array
import List

type alias Grid = Array.Array (Array.Array Bool)
type alias Dimensions = (Int, Int) -- (x,y)

--updateGrid : Dimensions -> Grid -> Grid

emptyGrid : Dimensions -> Grid
emptyGrid (x,y) = Array.fromList (List.repeat x (Array.fromList (List.repeat y False)))

getNeighborCount : Grid -> Int -> Int -> Int
getNeighborCount g x y = 
    List.sum (List.map (\val -> gridIntMap (getElement g val)) 
        [(x-1, y-1), (x-1, y), (x-1,y+1), (x+1, y-1), (x+1, y), (x+1,y+1), (x, y-1), (x,y+1)])

gridIntMap : Maybe Bool -> Int
gridIntMap x = case x of
    Nothing -> 0
    Just True -> 1
    Just False -> 0

getElement : Grid -> (Int, Int) -> Maybe Bool
getElement g (x,y) = 
    case (Array.get x g) of
        Nothing -> Nothing
        Just arr -> (Array.get y arr)