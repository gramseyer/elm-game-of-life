module StateControl (State, startState, changeMode, clearGrid, newGrid, processXChange, processYChange, tickUpdate, clickUpdate, liveToDeathUpdate, deadToLifeUpdate) where

import GameOfLife exposing (..)
import PresetStarts exposing (..)
import String

type alias State = {g : Grid, running : Bool, newCoords : (Maybe Int, Maybe Int), liveToDeath : List Int, deadToLife : List Int}

startState : State
startState = {g = gliderGun, running = False, newCoords = (Nothing, Nothing), liveToDeath = [0,1,4,5,6,7,8], deadToLife = [3]}

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
    {state | g = updateGrid state.liveToDeath state.deadToLife state.g}
  else
    state

clickUpdate : (Int, Int) -> State -> State
clickUpdate (coorx,coory) state = 
  if not state.running then 
    {state | g = toggleCoord (coorx, coory) state.g}
  else
    state

liveToDeathUpdate : Int -> Bool -> State -> State
liveToDeathUpdate val bool state =
  if Debug.log (toString (val, bool)) bool then
    if List.member val state.liveToDeath
    then
      state
    else
      {state | liveToDeath = val::state.liveToDeath}
  else
    if List.member val state.liveToDeath
    then
      {state | liveToDeath = List.filterMap (\x-> if x==val then Nothing else Just x) state.liveToDeath}
    else
      state

deadToLifeUpdate : Int -> Bool -> State -> State
deadToLifeUpdate val bool state =
  if Debug.log (toString (val, bool)) bool then
    if List.member val state.deadToLife
    then
      state
    else
      {state | deadToLife = val::state.deadToLife}
  else
    if List.member val state.liveToDeath
    then
      {state | deadToLife = List.filterMap (\x-> if x==val then Nothing else Just x) state.deadToLife}
    else
      state




