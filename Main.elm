module Main where
import StateControl exposing (..)
import GameOfLife exposing (..)
import Graphics.Element as E
import Graphics.Collage as C exposing(defaultLine)
import Graphics.Input as I
import Graphics.Input.Field as F
import Signal
import Color
import Array
import Time
import Window

type alias ClickEvent = (Int, Int)
type alias Event = (State -> State)

lastClicked : Signal.Mailbox ClickEvent
lastClicked = Signal.mailbox (0,0)

statechange : Signal.Mailbox Event
statechange = Signal.mailbox (identity)

fieldx : Signal.Mailbox F.Content
fieldx = Signal.mailbox F.noContent

fieldy : Signal.Mailbox F.Content
fieldy = Signal.mailbox F.noContent

renderXField : F.Content -> E.Element
renderXField = F.field F.defaultStyle (Signal.message fieldx.address)  "X-Dimension"

renderYField : F.Content -> E.Element
renderYField = F.field F.defaultStyle (Signal.message fieldy.address)  "Y-Dimension"

liveToDeathBoxes : List (Signal.Mailbox Bool)
liveToDeathBoxes = generateMailboxes startState.liveToDeath 

deadToLifeBoxes : List (Signal.Mailbox Bool)
deadToLifeBoxes = generateMailboxes startState.deadToLife

generateMailboxes : List Int -> List (Signal.Mailbox Bool)
generateMailboxes list = List.map (\num -> if List.member num list then Signal.mailbox True else Signal.mailbox False) [0..8]

renderBox : Signal.Mailbox Bool -> Bool -> E.Element
renderBox check bool = E.container 40 40 E.middle (I.checkbox (Signal.message check.address) bool)

renderBoxList : List (Signal.Mailbox Bool) -> Signal E.Element
renderBoxList checkBoxes = let listOfSignals = (List.map ((\x -> Signal.map (renderBox x) x.signal)) checkBoxes) in
  recursiveMerge listOfSignals

recursiveMerge : List (Signal E.Element) -> Signal E.Element
recursiveMerge signals = case signals of 
  x::[] -> x
  x::xs -> mergeTwoBoxes x (recursiveMerge xs)
  [] -> Debug.crash "recursiveMerge"

mergeTwoBoxes : Signal E.Element -> Signal E.Element -> Signal E.Element
mergeTwoBoxes x y = Signal.map2 (\x->\y-> E.flow E.right [x,y]) x y

liveToDeathBoxSignals : List (Signal Event)
liveToDeathBoxSignals = makeBoxSignals liveToDeathUpdate liveToDeathBoxes 

deadToLifeBoxSignals : List (Signal Event)
deadToLifeBoxSignals = makeBoxSignals deadToLifeUpdate deadToLifeBoxes 

makeBoxSignals : (Int -> Bool -> State -> State) -> List (Signal.Mailbox Bool) -> List (Signal Event)
makeBoxSignals func boxes = List.map2 (\int-> \box -> Signal.map (\bool -> func int bool) box.signal) [0..8] boxes

-- view will generate a grid of squares black -> false, white -> true
view : (Int, Int) -> State -> E.Element -> E.Element ->E.Element -> E.Element
view (w,h) state newGridFields liveToDeathChecks deadToLifeChecks = 
  let renderedUIElements = renderButtons (w,h) state in
  let uiElements = E.flow E.right (List.append renderedUIElements [newGridFields, (E.flow E.down [liveToDeathChecks, deadToLifeChecks])]) in
    E.flow E.down ((renderGrid (w,h) state) :: [uiElements])

findsqsizewh : (Int, Int) -> Grid -> (Float, Int, Int)
findsqsizewh (w,h) g = 
  let (maxx, maxy) = getDimensions g in 
--the size of the square should be the smallest of the window/sizeofarray
  let sqsize = Basics.min ((toFloat w)/(toFloat maxx)) ((toFloat (h - 100))/(toFloat maxy)) in 
  let width = ceiling (sqsize * (toFloat maxx))
      height = ceiling (sqsize * (toFloat maxy))
  in
    (sqsize, width, height)

renderGrid : (Int, Int) -> State -> E.Element
renderGrid (w,h) state = 
  let (sqsize, width, height) = findsqsizewh (w,h) state.g
  in 
  --create collage of appropriate size, not doing anything intelligent here yet and fill it with squares
  C.collage 
    width height
    ((C.filled Color.blue (C.rect (toFloat(width)) (toFloat(height)))) :: (List.concat (gridMapExtra makesquare (sqsize, width, height) state.g)))

renderButtons : (Int, Int) -> State -> List E.Element
renderButtons (w,h) state = 
    [ (I.button (Signal.message statechange.address changeMode) (getToggleButtonText state))
    , (I.button (Signal.message statechange.address clearGrid) "Clear Grid")
    , (I.button (Signal.message statechange.address newGrid) "New Grid") 
    ]

getToggleButtonText : State -> String
getToggleButtonText state = if state.running then "Stop Simulation" else "Start Simulation"

renderNewGridInputFields : F.Content -> F.Content -> E.Element
renderNewGridInputFields contentx contenty = 
  let renderedXField = renderXField contentx in
  let renderedYField = renderYField contenty in
  E.flow E.down [renderedXField, renderedYField]

makeSquareForm : Color.Color -> Float -> C.Form
makeSquareForm color size = (C.group [(C.filled color (C.square size)), (C.outlined {defaultLine | color = Color.blue} (C.square size)) ]) 

makeFormIntoClickable : Int -> Int -> Int ->C.Form -> C.Form
makeFormIntoClickable x y size form=
  C.toForm (I.clickable (Signal.message lastClicked.address (x,y)) (C.collage size size [form]))

makesquare : ((Int, Int), Bool, (Float, Int, Int)) -> C.Form
makesquare ((x,y), v, (size, maxx, maxy)) = 
  let coords = ((0.5+(toFloat x)) *size - ((toFloat maxx)/2) , (0.5+ (toFloat y)) *size - ((toFloat maxy)/2)) in
  if v then
    C.move coords (makeFormIntoClickable x y (round size) (makeSquareForm Color.white size))
  else 
    C.move coords (makeFormIntoClickable x y (round size) (makeSquareForm Color.black size))

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
main = Signal.map5 view Window.dimensions
 (Signal.foldp upstate startState eventSignal) (Signal.map2 renderNewGridInputFields fieldx.signal fieldy.signal) (renderBoxList liveToDeathBoxes) (renderBoxList deadToLifeBoxes)