module Main where
import StateControl exposing (..)
import Render exposing (..)
import Graphics.Element as E
import Graphics.Input.Field as F
import Signal
import Time
import Window

lastClicked : Signal.Mailbox ClickEvent
lastClicked = Signal.mailbox (0,0)

statechange : Signal.Mailbox Event
statechange = Signal.mailbox (identity)

fieldx : Signal.Mailbox F.Content
fieldx = Signal.mailbox F.noContent

fieldy : Signal.Mailbox F.Content
fieldy = Signal.mailbox F.noContent

liveToDeathBoxes : List (Signal.Mailbox Bool)
liveToDeathBoxes = generateMailboxes startState.liveToDeath 

deadToLifeBoxes : List (Signal.Mailbox Bool)
deadToLifeBoxes = generateMailboxes startState.deadToLife

generateMailboxes : List Int -> List (Signal.Mailbox Bool)
generateMailboxes list = List.map (\num -> if List.member num list then Signal.mailbox True else Signal.mailbox False) [0..8]

liveToDeathBoxSignals : List (Signal Event)
liveToDeathBoxSignals = makeBoxSignals liveToDeathUpdate liveToDeathBoxes 

deadToLifeBoxSignals : List (Signal Event)
deadToLifeBoxSignals = makeBoxSignals deadToLifeUpdate deadToLifeBoxes 

makeBoxSignals : (Int -> Bool -> State -> State) -> List (Signal.Mailbox Bool) -> List (Signal Event)
makeBoxSignals func boxes = List.map2 (\int-> \box -> Signal.map (\bool -> func int bool) box.signal) [0..8] boxes

xDimensionSignal : Signal Event
xDimensionSignal = (Signal.map (\content ->  (processXChange content.string)) fieldx.signal)

yDimensionSignal : Signal Event
yDimensionSignal = (Signal.map (\content ->  (processYChange content.string)) fieldy.signal)

tickSignal : Signal Event
tickSignal = Signal.map (\x-> tickUpdate) (Time.every (Time.millisecond*500))

clickSignal : Signal Event
clickSignal = Signal.map (\(x,y)-> (clickUpdate (x,y)) ) lastClicked.signal

eventSignal : Signal Event
eventSignal = (Signal.mergeMany (List.append [tickSignal, clickSignal, statechange.signal, xDimensionSignal, yDimensionSignal] (List.append liveToDeathBoxSignals deadToLifeBoxSignals)))

upstate : Event -> State -> State
upstate t state = (t state)

main : Signal E.Element
main = Signal.map5 (view statechange lastClicked) Window.dimensions
 (Signal.foldp upstate startState eventSignal) (Signal.map2 (renderNewGridInputFields fieldx fieldy) fieldx.signal fieldy.signal) (renderBoxList liveToDeathBoxes) (renderBoxList deadToLifeBoxes)