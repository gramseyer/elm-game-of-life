module GameOfLife where

import Array
import List

type alias Grid = Array.Array (Array.Array Bool)
type alias Dimensions = (Int, Int) -- (x,y)

updateGrid : Dimensions -> List Int -> List Int -> Grid -> Grid
updateGrid (xMax,yMax) liveToDeath deadToLife g =
    fromListList (gridMap (\((x,y),bool)-> (chooseNextState (bool, getNeighborCount g x y) liveToDeath deadToLife)) g)

coordList : Dimensions -> List (List (Int, Int))
coordList (xMax, yMax) = 
    (List.map (\f->List.map f [0..yMax]) (List.map (\x->(\y->(x,y))) [0..xMax]))

fromListList : List (List Bool) -> Grid
fromListList xs = Array.fromList (List.map (Array.fromList) xs)

emptyGrid : Dimensions -> Grid
emptyGrid (x,y) = fromListList (List.repeat x (List.repeat y False))

getNeighborCount : Grid -> Int -> Int -> Int
getNeighborCount g x y = 
    List.sum (List.map (\val -> gridIntMap (getElement g val)) 
        [(x-1, y-1), (x-1, y), (x-1,y+1), (x+1, y-1), (x+1, y), (x+1,y+1), (x, y-1), (x,y+1)])

getAlive : Maybe Bool -> Bool
getAlive x = case x of
    Nothing -> False
    Just y -> y

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

chooseNextState : (Bool, Int) -> List Int -> List Int -> Bool
chooseNextState (prevState, neighbors) liveToDeath deadToLife = 
    if prevState then --alive
        if List.member neighbors liveToDeath then
            False
        else
            True
    else --dead
        if List.member neighbors deadToLife then
            True
        else
            False

gridMap : (((Int,Int), Bool)->b) -> Grid -> List(List b)
gridMap f g =
    ( (List.map (\(x, xArray) -> (List.map (\(y, bool) -> (f ((x,y), bool))) (Array.toIndexedList xArray))) (Array.toIndexedList g) ))

