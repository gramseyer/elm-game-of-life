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
type Event = Tick | Click (Int, Int) | UIStateChange (State -> State) 

lastClicked : Signal.Mailbox ClickEvent
lastClicked = Signal.mailbox (0,0)

statechange : Signal.Mailbox Event
statechange = Signal.mailbox (UIStateChange identity)

fieldx : Signal.Mailbox F.Content
fieldx = Signal.mailbox F.noContent


-- view will generate a grid of squares black -> false, white -> true
view : (Int, Int) -> State -> F.Content -> E.Element
view (w,h) state content = 
  let renderedUIElements = renderButtons (w,h) state in
  let renderedXField = realXField content in
  let uiElements = E.flow E.right (List.append renderedUIElements [renderedXField]) in
  E.flow E.down ((renderGrid (w,h) state) :: [uiElements])

findsqsizewh : (Int, Int) -> Grid -> (Float, Int, Int)
findsqsizewh (w,h) g = 
 let (maxx, maxy) = getDimensions g
 in 
--the size of the square should be the smallest of the window/sizeofarray
 let sqsize = Basics.min ((toFloat w)/(toFloat maxx)) ((toFloat (h - 100))/(toFloat maxy))
 in 
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
   let (sqsize, wc, hc) = findsqsizewh (w,h) state.g
   in
   ((((I.button (Signal.message statechange.address (UIStateChange changeMode)))) "Change Mode") :: 
    [ (I.button (Signal.message statechange.address (UIStateChange clearGrid)) "Clear Grid"), 
      (I.button (Signal.message statechange.address (UIStateChange newGrid)) "New Grid") ])



realXField : F.Content -> E.Element
realXField = F.field F.defaultStyle (Signal.message fieldx.address)  "X-Dimension"

makeSquareForm : Color.Color -> Float -> C.Form
makeSquareForm color size = (C.group [(C.filled color (C.square size)), (C.outlined {defaultLine | color = Color.blue} (C.square size)) ]) 

makeFormIntoClickable : Int -> Int -> Int ->C.Form -> C.Form
makeFormIntoClickable x y size form=
    C.toForm (I.clickable (Signal.message lastClicked.address (x,y)) (C.collage size size [form]))

makesquare : ((Int, Int), Bool, (Float, Int, Int)) -> C.Form
makesquare ((x,y), v, (size, maxx, maxy))= 
 if v then
  C.move ((0.5+(toFloat x)) *size - ((toFloat maxx)/2) , (0.5+ (toFloat y)) *size - ((toFloat maxy)/2)) (makeFormIntoClickable x y (round size) (makeSquareForm Color.white size))
 else 
  C.move (((0.5+(toFloat x)) *size - ((toFloat maxx)/2)) , (0.5+(toFloat y)) *size - ((toFloat maxy)/2)) (makeFormIntoClickable x y (round size) (makeSquareForm Color.black size))

-- List.map (\w-> List.map (\v -> (v, Array.get (snd v) ((Array.get (fst v) grid))))) (List.map (\f->List.map f [0..yMax]) (List.map (\x->(\y->(x,y))) [0..xMax]))

xDimensionSignal : Signal Event
xDimensionSignal = (Signal.map (\content -> UIStateChange (processXChange content.string)) fieldx.signal)

tickSignal : Signal Event
tickSignal = (Signal.map (\x-> Tick) (Time.every (Time.millisecond*500)))

eventSignal : Signal Event
eventSignal = (Signal.mergeMany [tickSignal, (Signal.map (\(x,y)-> Click (x,y)) lastClicked.signal), statechange.signal, xDimensionSignal])

upstate : Event -> State -> State
upstate t state = 
    case t of
        Tick -> if state.running then {state | g = updateGrid [0,1,4,5,6,7,8] [3] state.g}
                        else state
        UIStateChange f -> (f state)
        Click (coorx,coory) -> if not state.running then {state | g = toggleCoord (coorx, coory) state.g}
                        else state


main : Signal E.Element
main = Signal.map3 view Window.dimensions
 (Signal.foldp upstate startState eventSignal) fieldx.signal