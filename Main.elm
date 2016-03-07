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

saveGridNameField : Signal.Mailbox F.Content
saveGridNameField = Signal.mailbox F.noContent

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
newGridXDimensionSignal = makeFieldSignal processNewXChange newGridFieldx

newGridYDimensionSignal : Signal Event
newGridYDimensionSignal = makeFieldSignal processNewYChange newGridFieldy

maxGridXDimensionSignal : Signal Event
maxGridXDimensionSignal = makeFieldSignal processMaxXChange maxGridFieldx

maxGridYDimensionSignal : Signal Event
maxGridYDimensionSignal = makeFieldSignal processMaxYChange maxGridFieldy

saveGridNameEventSignal : Signal Event
saveGridNameEventSignal = makeFieldSignal storeSaveStringUpdate saveGridNameField

makeFieldSignal : (String -> Event) -> Signal.Mailbox F.Content -> Signal Event
makeFieldSignal func mailbox = Signal.map (\content -> func content.string) mailbox.signal

timeSignal : Signal Float
timeSignal = Signal.map (Time.inMilliseconds) (Time.every (Time.millisecond * 50))

--rawTickSignal : Signal ()
--rawTickSignal = Signal.map (\_ -> ()) (Time.every (Time.millisecond*50))

tickSignal : Signal Event
tickSignal = Signal.map maybeTickUpdate timeSignal

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
                                              , saveGridNameEventSignal
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

saveGridNameElementSignal : Signal E.Element
saveGridNameElementSignal = Signal.map (renderField saveGridNameField "Save Grid As") saveGridNameField.signal

-- Combining Signals

map7 : (a -> b -> c -> d -> e -> f -> g -> result) 
      -> Signal a
      -> Signal b
      -> Signal c
      -> Signal d
      -> Signal e
      -> Signal f
      -> Signal g
      -> Signal result
map7 f sigA sigB sigC sigD sigE sigF sigG = 
  Signal.map3 (\func -> \val1 ->\val2 -> func val1 val2) (Signal.map5 f sigA sigB sigC sigD sigE) sigF sigG

upstate : Event -> State -> State
upstate t state = (t state)

main : Signal E.Element
main = map7 (view statechange lastClicked) 
              Window.dimensions
              (Signal.foldp upstate startState eventSignal) 
              newGridFieldsSignal
              liveToDeathBoxSignal
              deadToLifeBoxSignal
              maxGridFieldsSignal
              saveGridNameElementSignal