module GameOfLife (Grid, Toroidal, updateGrid, gridMap, gridMapExtra, emptyGrid, getElement, fromListList, getDimensions, toggleCoord) where

import Array
import List
import Debug

--Our basic type for the grid is an array of arrays
type alias Grid = Array.Array (Array.Array Bool)
-- the dimensions of the grid
type alias Dimensions = (Int, Int) -- (x,y)
--whether or not we "wrap around"
type alias Toroidal = Bool

--based on our current rules update our grid one state
updateGrid : Toroidal -> (Maybe Int, Maybe Int) -> List Int -> List Int -> Grid -> Grid
updateGrid toroidal caps liveToDeath deadToLife g =
    let (xMax, yMax) = getDimensions g in
    expandGrid toroidal caps (fromListList (gridMap (\((x,y),bool)-> (chooseNextState (bool, getNeighborCount toroidal g x y) liveToDeath deadToLife)) g))

--turn a list of lists into an array of arrays to be cast as a grid
fromListList : List (List Bool) -> Grid
fromListList xs = Array.fromList (List.map (Array.fromList) xs)

-- generate an empty grid with the given dimensions
emptyGrid : Dimensions -> Grid
emptyGrid (x,y) = fromListList (List.repeat x (List.repeat y False))

-- for a given point find out how many neighbors there are
getNeighborCount : Toroidal -> Grid -> Int -> Int -> Int
getNeighborCount toroidal g x y = 
    List.sum (List.map (\val -> gridIntMap (getElement toroidal g val)) 
        [(x-1, y-1), (x-1, y), (x-1,y+1), (x+1, y-1), (x+1, y), (x+1,y+1), (x, y-1), (x,y+1)])

gridIntMap : Maybe Bool -> Int
gridIntMap x = case x of
    Nothing -> 0
    Just True -> 1
    Just False -> 0

-- find the value of a square
getElement : Toroidal -> Grid -> (Int, Int) -> Maybe Bool
getElement toroidal g (x,y) =
  let (maxX, maxY) = getDimensions g in 
  let xcoor = if toroidal then x%maxX else x
      ycoor = if toroidal then y%maxY else y in
    case (Array.get xcoor g) of
        Nothing -> Nothing
        Just arr -> (Array.get ycoor arr)

-- calculate next alive dead state
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

-- map changes over the whole grid
gridMap : (((Int, Int), Bool)->b) -> Grid -> List(List b)
gridMap f g = gridMapExtra (\((x,y), bool, other)-> f ((x,y), bool)) 0 g

-- map changes over the whole grid including an optional extra argument (or set of arguments)
gridMapExtra : (((Int,Int), Bool, other)->b) -> other -> Grid -> List(List b)
gridMapExtra f goo g =
  ((List.map 
    (\(x, xArray) -> 
      (List.map 
        (\(y, bool) -> (f ((x,y), bool, goo))) 
        (Array.toIndexedList xArray))) 
    (Array.toIndexedList g)))

-- get the grid dimensions, assumes a rectangular shape
getDimensions : Grid -> Dimensions
getDimensions g = (Array.length g, getMax (List.maximum (List.map Array.length (Array.toList g))))

getMax : Maybe (Int) -> Int
getMax x = case x of
  Nothing -> 0
  Just x  -> x

-- Swap the value of a grid point in place
toggleCoord : (Int, Int) -> Grid -> Grid
toggleCoord (x,y) g = 
  case Array.get x g of
    Nothing -> g
    Just arr -> case Array.get y arr of
                    Nothing -> g
                    Just bool -> Array.set x (Array.set y (not bool) arr) g

-- expand the grid on the left column
addColumnLeft : Int -> Grid -> Grid
addColumnLeft count g = let (xMax, yMax) = getDimensions g in
    let newColumn = Array.repeat yMax False in
    if count > 0 then
      Array.append (Array.fromList [newColumn]) (addColumnLeft (count-1) g) 
    else
      g
-- expand the grid on the right column
addColumnRight : Int -> Grid -> Grid
addColumnRight count g = let (xMax, yMax) = getDimensions g in
    let newColumn = Array.repeat yMax False in
    if count > 0 then
      Array.append (addColumnRight (count-1) g) (Array.fromList [newColumn])
    else 
      g
-- add a row on the bottom to the grid
addRowBot : Int -> Grid -> Grid
addRowBot count g = Array.map (\arr -> Array.append (Array.repeat count False) arr) g

-- add a row on the top to the grid
addRowTop : Int -> Grid -> Grid
addRowTop count g = Array.map (\arr -> Array.append arr (Array.repeat count False)) g

-- expand the grid based upon the settings of our state and the direction of expansion necessary
conditionalExpand : (Int -> Grid -> Grid) 
                  -> ((Maybe Int, Maybe Int)-> Grid -> Int)
                  -> (Grid -> Bool)
                  -> (Maybe Int, Maybe Int)
                  -> Grid
                  -> Grid
conditionalExpand expand_f cap_f condition_f state g = 
  if condition_f g then
    expand_f (cap_f state g) g
  else g

-- simultaneously expand 3 rows, otherwise we refresh constantly 
maxRowsToExpand : Int
maxRowsToExpand = 3

-- expand by at most maxRowsToExpand depending on the cap the user has set for maximum size
rowsToExpandHorizontally: (Maybe Int, Maybe Int) -> Grid -> Int
rowsToExpandHorizontally caps grid =
  let (xMax, yMax) = getDimensions grid in
  case fst (caps) of 
    Just cap -> max (min maxRowsToExpand (cap-xMax)) 0
    Nothing -> maxRowsToExpand

-- expand by at most maxRowsToExpand depending on the cap the user has set for maximum size
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

-- determine if we need to expand left
expandToLeft : Grid -> Bool
expandToLeft g = let (xMax, yMax) = getDimensions g in
  List.member True (Array.toList (fromJust (Array.get 0 g)))

-- determine if we need to expand right
expandToRight : Grid -> Bool
expandToRight g = let (xMax, yMax) = getDimensions g in
  List.member True (Array.toList (fromJust (Array.get (xMax-1) g)))

-- determine if we need to expand down
expandToBot : Grid -> Bool
expandToBot g = let (xMax, yMax) = getDimensions g in
  List.member True (Array.toList (Array.map (\arr -> (fromJust (Array.get 0 arr))) g))

-- determine if we need to expand up
expandToTop : Grid -> Bool
expandToTop g = let (xMax, yMax) = getDimensions g in
  List.member True (Array.toList (Array.map (\arr -> (fromJust (Array.get (yMax-1) arr))) g))

--expand the grid based on our needs
expandGrid : Toroidal -> (Maybe Int, Maybe Int) -> Grid -> Grid
expandGrid toroidal caps g = if toroidal then g else 
    g |> conditionalExpand addColumnLeft rowsToExpandHorizontally expandToLeft caps
      |> conditionalExpand addColumnRight rowsToExpandHorizontally expandToRight caps
      |> conditionalExpand addRowBot rowsToExpandVertically expandToBot caps
      |> conditionalExpand addRowTop rowsToExpandVertically expandToTop caps


--Probably no longer necessary
coordList : Dimensions -> List (List (Int, Int))
coordList (xMax, yMax) = 
    (List.map (\f->List.map f [0..yMax]) (List.map (\x->(\y->(x,y))) [0..xMax]))
