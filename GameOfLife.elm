module GameOfLife where

import Array
import List

type alias Grid = Array.Array (Array.Array Bool)
type alias Dimensions = (Int, Int) -- (x,y)

updateGrid : Dimensions -> List Int -> List Int -> Grid -> Grid
updateGrid (xMax,yMax) liveToDeath deadToLife g =
    fromListList ((List.map (\pairs -> List.map (\(x,y)-> chooseNextState (getNeighborCount g x y) liveToDeath deadToLife) pairs) (coordList (xMax, yMax))))

coordList : Dimensions -> List (List (Int, Int))
coordList (xMax, yMax) = 
    (List.map (\f->List.map f [0..yMax]) (List.map (\x->(\y->(x,y))) [0..xMax]))


fromListList : List (List Bool) -> Grid
fromListList xs = Array.fromList (List.map (Array.fromList) xs)

emptyGrid : Dimensions -> Grid
emptyGrid (x,y) = fromListList (List.repeat x (List.repeat y False))

getNeighborCount : Grid -> Int -> Int -> (Bool, Int)
getNeighborCount g x y = 
    (getAlive (getElement g (x,y)), List.sum (List.map (\val -> gridIntMap (getElement g val)) 
        [(x-1, y-1), (x-1, y), (x-1,y+1), (x+1, y-1), (x+1, y), (x+1,y+1), (x, y-1), (x,y+1)]))

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
