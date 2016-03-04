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


parse : String -> Maybe Int
parse x = Debug.log x (Result.toMaybe (String.toInt x))
