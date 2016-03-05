module Main where
import StateControl exposing (..)
import Render exposing (..)
import Graphics.Element as E
import Graphics.Input.Field as F
import Signal
import Time
import Window

-- Mailboxes

lastClicked : Signal.Mailbox ClickEvent
lastClicked = Signal.mailbox (0,0)

statechange : Signal.Mailbox Event
statechange = Signal.mailbox (identity)

newGridFieldx : Signal.Mailbox F.Content
newGridFieldx = Signal.mailbox F.noContent

newGridFieldy : Signal.Mailbox F.Content
newGridFieldy = Signal.mailbox F.noContent

maxGridFieldx : Signal.Mailbox F.Content
maxGridFieldx = Signal.mailbox F.noContent

maxGridFieldy : Signal.Mailbox F.Content
maxGridFieldy = Signal.mailbox F.noContent

liveToDeathBoxes : List (Signal.Mailbox Bool)
liveToDeathBoxes = generateMailboxes startState.liveToDeath 

deadToLifeBoxes : List (Signal.Mailbox Bool)
deadToLifeBoxes = generateMailboxes startState.deadToLife

generateMailboxes : List Int -> List (Signal.Mailbox Bool)
generateMailboxes list = List.map (\num -> if List.member num list then Signal.mailbox True else Signal.mailbox False) [0..8]

-- Event Signals

liveToDeathBoxSignals : List (Signal Event)
liveToDeathBoxSignals = makeBoxSignals liveToDeathUpdate liveToDeathBoxes 

deadToLifeBoxSignals : List (Signal Event)
deadToLifeBoxSignals = makeBoxSignals deadToLifeUpdate deadToLifeBoxes 

makeBoxSignals : (Int -> Bool -> State -> State) -> List (Signal.Mailbox Bool) -> List (Signal Event)
makeBoxSignals func boxes = List.map2 (\int-> \box -> Signal.map (\bool -> func int bool) box.signal) [0..8] boxes

newGridXDimensionSignal : Signal Event
newGridXDimensionSignal = (Signal.map (\content ->  (processNewXChange content.string)) newGridFieldx.signal)

newGridYDimensionSignal : Signal Event
newGridYDimensionSignal = (Signal.map (\content ->  (processNewYChange content.string)) newGridFieldy.signal)

maxGridXDimensionSignal : Signal Event
maxGridXDimensionSignal = (Signal.map (\content ->  (processMaxXChange content.string)) maxGridFieldx.signal)

maxGridYDimensionSignal : Signal Event
maxGridYDimensionSignal = (Signal.map (\content ->  (processMaxYChange content.string)) maxGridFieldy.signal)

tickSignal : Signal Event
tickSignal = Signal.map (\x-> tickUpdate) (Time.every (Time.millisecond*500))

clickSignal : Signal Event
clickSignal = Signal.map (\(x,y)-> (clickUpdate (x,y)) ) lastClicked.signal

eventSignal : Signal Event
eventSignal = (Signal.mergeMany (List.append [  tickSignal
                                              , clickSignal
                                              , statechange.signal
                                              , newGridXDimensionSignal
                                              , newGridYDimensionSignal
                                              , maxGridXDimensionSignal
                                              , maxGridYDimensionSignal
                                              ] 
                                              (List.append liveToDeathBoxSignals deadToLifeBoxSignals)))

-- UI Element Signals

newGridFieldsSignal : Signal E.Element
newGridFieldsSignal = Signal.map2 (renderNewGridInputFields newGridFieldx newGridFieldy)
                                      newGridFieldx.signal newGridFieldy.signal

maxGridFieldsSignal : Signal E.Element
maxGridFieldsSignal = Signal.map2 (renderMaxGridInputFields maxGridFieldx maxGridFieldy) 
                                      maxGridFieldx.signal maxGridFieldy.signal

liveToDeathBoxSignal : Signal E.Element
liveToDeathBoxSignal = renderBoxList liveToDeathBoxes

deadToLifeBoxSignal : Signal E.Element
deadToLifeBoxSignal = renderBoxList deadToLifeBoxes

-- Combining Signals

map6 : (a -> b -> c -> d -> e -> f -> result) 
      -> Signal a
      -> Signal b
      -> Signal c
      -> Signal d
      -> Signal e
      -> Signal f
      -> Signal result
map6 f sigA sigB sigC sigD sigE sigF = 
  Signal.map2 (\func -> \val -> func val) (Signal.map5 f sigA sigB sigC sigD sigE) sigF

upstate : Event -> State -> State
upstate t state = (t state)

main : Signal E.Element
main = map6 (view statechange lastClicked) 
              Window.dimensions
              (Signal.foldp upstate startState eventSignal) 
              newGridFieldsSignal
              liveToDeathBoxSignal
              deadToLifeBoxSignal
              maxGridFieldsSignal