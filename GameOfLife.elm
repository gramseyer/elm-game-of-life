module GameOfLife (Grid, updateGrid, gridMap, gridMapExtra, emptyGrid, getElement, fromListList, getDimensions, toggleCoord) where

import Array
import List
import Debug

type alias Grid = Array.Array (Array.Array Bool)
type alias Dimensions = (Int, Int) -- (x,y)

updateGrid : (Maybe Int, Maybe Int) -> List Int -> List Int -> Grid -> Grid
updateGrid caps liveToDeath deadToLife g =
    let (xMax, yMax) = getDimensions g in
    expandGrid caps (fromListList (gridMap (\((x,y),bool)-> (chooseNextState (bool, getNeighborCount g x y) liveToDeath deadToLife)) g))

fromListList : List (List Bool) -> Grid
fromListList xs = Array.fromList (List.map (Array.fromList) xs)

emptyGrid : Dimensions -> Grid
emptyGrid (x,y) = fromListList (List.repeat x (List.repeat y False))

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

gridMap : (((Int, Int), Bool)->b) -> Grid -> List(List b)
gridMap f g = gridMapExtra (\((x,y), bool, other)-> f ((x,y), bool)) 0 g

gridMapExtra : (((Int,Int), Bool, other)->b) -> other -> Grid -> List(List b)
gridMapExtra f goo g =
  ((List.map 
    (\(x, xArray) -> 
      (List.map 
        (\(y, bool) -> (f ((x,y), bool, goo))) 
        (Array.toIndexedList xArray))) 
    (Array.toIndexedList g)))

getDimensions : Grid -> Dimensions
getDimensions g = (Array.length g, getMax (List.maximum (List.map Array.length (Array.toList g))))

getMax : Maybe (Int) -> Int
getMax x = case x of
  Nothing -> 0
  Just x  -> x

toggleCoord : (Int, Int) -> Grid -> Grid
toggleCoord (x,y) g = 
  case Array.get x g of
    Nothing -> g
    Just arr -> case Array.get y arr of
                    Nothing -> g
                    Just bool -> Array.set x (Array.set y (not bool) arr) g

addColumnLeft : Int -> Grid -> Grid
addColumnLeft count g = let (xMax, yMax) = getDimensions g in
    let newColumn = Array.repeat yMax False in
    Array.append (Array.repeat count newColumn) g

addColumnRight : Int -> Grid -> Grid
addColumnRight count g = let (xMax, yMax) = getDimensions g in
    let newColumn = Array.repeat yMax False in
    Array.append g (Array.repeat count newColumn)

addRowTop : Int -> Grid -> Grid
addRowTop count g = Array.map (\arr -> Array.append (Array.repeat count False) arr) g

addRowBot : Int -> Grid -> Grid
addRowBot count g = Array.map (\arr -> Array.append arr (Array.repeat count False)) g

conditionalExpand : (Int -> Grid -> Grid) -> ((Maybe Int, Maybe Int) -> Grid -> Int) -> (Grid -> Bool) -> (Maybe Int, Maybe Int) -> Grid -> Grid
conditionalExpand expand_f cap_f condition_f state g = 
  if condition_f g then
    expand_f (cap_f state g) g
  else g

maxRowsToExpand : Int
maxRowsToExpand = 3

rowsToExpandHorizontally: (Maybe Int, Maybe Int) -> Grid -> Int
rowsToExpandHorizontally caps grid =
  let (xMax, yMax) = getDimensions grid in
  case fst (caps) of 
    Just cap -> max (min maxRowsToExpand (cap-xMax)) 0
    Nothing -> maxRowsToExpand

rowsToExpandVertically: (Maybe Int, Maybe Int) -> Grid -> Int
rowsToExpandVertically caps grid =
  let (xMax, yMax) = getDimensions grid in
  case snd (caps) of 
    Just cap -> max (min maxRowsToExpand (cap-yMax)) 0
    Nothing -> maxRowsToExpand

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "fromJust"

expandToLeft : Grid -> Bool
expandToLeft g = let (xMax, yMax) = getDimensions g in
  List.member True (Array.toList (fromJust (Array.get 0 g)))

expandToRight : Grid -> Bool
expandToRight g = let (xMax, yMax) = getDimensions g in
  List.member True (Array.toList (fromJust (Array.get (xMax-1) g)))

expandToTop : Grid -> Bool
expandToTop g = let (xMax, yMax) = getDimensions g in
  List.member True (Array.toList (Array.map (\arr -> (fromJust (Array.get 0 arr))) g))

expandToBot : Grid -> Bool
expandToBot g = let (xMax, yMax) = getDimensions g in
  List.member True (Array.toList (Array.map (\arr -> (fromJust (Array.get (yMax-1) arr))) g))

expandGrid : (Maybe Int, Maybe Int) -> Grid -> Grid
expandGrid caps g = 
    g |> conditionalExpand addColumnLeft rowsToExpandHorizontally expandToLeft caps
      |> conditionalExpand addColumnRight rowsToExpandHorizontally expandToRight caps
      |> conditionalExpand addRowTop rowsToExpandVertically expandToTop caps
      |> conditionalExpand addRowBot rowsToExpandVertically expandToBot caps

--Probably no longer necessary
coordList : Dimensions -> List (List (Int, Int))
coordList (xMax, yMax) = 
    (List.map (\f->List.map f [0..yMax]) (List.map (\x->(\y->(x,y))) [0..xMax]))
