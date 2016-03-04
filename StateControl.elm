module StateControl where

import GameOfLife exposing (..)
import PresetStarts exposing (..)
import String

type alias State = {g : Grid, running : Bool, newCoords : (Maybe Int, Maybe Int)}

startState : State
startState = {g = gliderGun, running = False, newCoords = (Nothing, Nothing) }

changeMode : State -> State
changeMode state = { state |  running = not (state.running) } 

clearGrid : State -> State
clearGrid state =  {state | g = emptyGrid (getDimensions state.g) } 

newGrid : State -> State
newGrid state=  {state | g = emptyGrid (newDimensions state)}

newDimensions : State -> (Int, Int)
newDimensions state = case state.newCoords of
 (Just x', Just y') -> if x' == 0 || y' == 0 then 
                          (getDimensions state.g)
                       else 
                          (x',y')
 _ -> (getDimensions state.g)

processXChange : String -> State -> State
processXChange str state = {state | newCoords = (parse str, snd (state.newCoords)) }

processYChange : String -> State -> State
processYChange str state = {state | newCoords = (fst (state.newCoords), parse str) }

parse : String -> Maybe Int
parse x = Debug.log x (Result.toMaybe (String.toInt x))

tickUpdate : State -> State
tickUpdate state = 
  if state.running then 
    {state | g = updateGrid [0,1,4,5,6,7,8] [3] state.g}
  else
    state

clickUpdate : (Int, Int) -> State -> State
clickUpdate (coorx,coory) state = 
  if not state.running then 
    {state | g = toggleCoord (coorx, coory) state.g}
  else
    state

