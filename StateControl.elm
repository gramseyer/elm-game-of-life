module StateControl (State
                    , Event
                    , ClickEvent
                    , startState
                    , changeMode
                    , clearGrid
                    , newGrid
                    , processNewXChange
                    , processNewYChange
                    , processMaxXChange
                    , processMaxYChange
                    , maybeTickUpdate
                    , clickUpdate
                    , liveToDeathUpdate
                    , deadToLifeUpdate
                    , loadGridUpdate
                    , storeSaveStringUpdate
                    , saveGrid
                    , speedIncreaseUpdate
                    , speedDecreaseUpdate
                    ) where

import GameOfLife exposing (..)
import PresetStarts exposing (..)
import String

type alias State = {  g : Grid
                    , running : Bool
                    , maxSize : (Maybe Int, Maybe Int)
                    , newCoords : (Maybe Int, Maybe Int)
                    , liveToDeath : List Int
                    , deadToLife : List Int
                    , savedGrids : List (String, Grid)
                    , saveNameString : String
                    , lastUpdate : Float
                    , updatePeriod : Float
                  }

type alias ClickEvent = (Int, Int)
type alias Event = (State -> State)

startState : State
startState = { g = gliderGun
              , running = False
              , maxSize = (Nothing, Nothing)
              , newCoords = (Nothing, Nothing)
              , liveToDeath = [0,1,4,5,6,7,8]
              , deadToLife = [3]
              , savedGrids = [("Glider Gun", gliderGun)]
              , saveNameString = ""
              , lastUpdate = 0.0
              , updatePeriod = 100.0
            }

changeMode : State -> State
changeMode state = { state |  running = not (state.running) } 

clearGrid : State -> State
clearGrid state =  {state | g = emptyGrid (getDimensions state.g) , running = False} 

newGrid : State -> State
newGrid state=  {state | g = emptyGrid (newDimensions state), running = False}

newDimensions : State -> (Int, Int)
newDimensions state = case state.newCoords of
  (Just x', Just y') -> if x' == 0 || y' == 0 then 
                          (getDimensions state.g)
                        else 
                          (x',y')
  _ -> (getDimensions state.g)

processNewXChange : String -> State -> State
processNewXChange str state = {state | newCoords = (parse str, snd (state.newCoords)) }

processNewYChange : String -> State -> State
processNewYChange str state = {state | newCoords = (fst (state.newCoords), parse str) }

processMaxXChange : String -> State -> State
processMaxXChange str state = {state | maxSize = (parse str, snd (state.maxSize)) }

processMaxYChange : String -> State -> State
processMaxYChange str state = {state | maxSize = (fst (state.maxSize), parse str) }


parse : String -> Maybe Int
parse x = Debug.log x (Result.toMaybe (String.toInt x))

maybeTickUpdate : Float -> State -> State
maybeTickUpdate x state = if x > state.updatePeriod + state.lastUpdate then 
                            tickUpdate {state | lastUpdate = x}
                          else 
                            state

tickUpdate : State -> State
tickUpdate state = 
  if state.running then 
    {state | g = updateGrid state.maxSize state.liveToDeath state.deadToLife state.g}
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
  if Debug.log (toString state.liveToDeath) bool then
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
  if Debug.log (toString state.deadToLife) bool then
    if List.member val state.deadToLife
    then
      state
    else
      {state | deadToLife = val::state.deadToLife}
  else
    if List.member val state.deadToLife
    then
      {state | deadToLife = List.filterMap (\x-> if x==val then Nothing else Just x) state.deadToLife}
    else
      state

getSavedState : State -> String -> Grid
getSavedState state str = case getSavedStateFromList str state.savedGrids of
  Just a -> a
  Nothing -> Debug.crash "getSavedState"

getSavedStateFromList : String -> List (String, a) -> Maybe a
getSavedStateFromList str list = case list of
  [] -> Nothing
  (str2, a)::list' -> if str == str2 then Just a else (getSavedStateFromList str list')

loadGridUpdate : String -> State -> State
loadGridUpdate str state = Debug.log "foobar" { state | g = getSavedState state str, running=False}

storeSaveStringUpdate : String -> State -> State
storeSaveStringUpdate str state = { state | saveNameString = str}

saveGrid : State -> State
saveGrid state = case getSavedStateFromList state.saveNameString state.savedGrids of
  Nothing -> { state | savedGrids = (state.saveNameString, state.g) :: state.savedGrids}
  Just a -> state

speedUpdateIncrement : Float
speedUpdateIncrement = 50.0

speedIncreaseUpdate : State -> State
speedIncreaseUpdate state = if state.updatePeriod > speedUpdateIncrement then 
                              {state | updatePeriod = state.updatePeriod - speedUpdateIncrement}
                            else
                              state

speedDecreaseUpdate : State -> State
speedDecreaseUpdate state = {state | updatePeriod = state.updatePeriod + speedUpdateIncrement}