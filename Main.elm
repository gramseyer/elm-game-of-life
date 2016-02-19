module Main where
import GameOfLife exposing(..)
import PresetStarts exposing(..)
import Graphics.Element as E
import Graphics.Collage as C exposing(defaultLine)
import Graphics.Input as I
import Signal
import Color
import Array
import Time
import Window

--type alias State = (Grid, Bool) --this depends on the state of the game we're in, ie editing versus viewing mode

type alias ClickEvent = (Int, Int)
type Event = Tick | Click (Int, Int)

lastClicked : Signal.Mailbox ClickEvent
lastClicked = Signal.mailbox (0,0)

maxGridX = 3
maxGridY = 3

getDimensions : Grid -> (Int, Int)
getDimensions g = (Array.length g, Array.length (fromJust (Array.get 0 g)))

-- view will generate a grid of squares black -> false, white -> true
view : (Int, Int) -> Grid -> E.Element
view (w,h) g = 
-- if state then --if we are in viewing mode
 let (maxx, maxy) = getDimensions g
 in 
--the size of the square should be the smallest of the window/sizeofarray
 let sqsize = Basics.min ((toFloat w)/(toFloat maxx)) ((toFloat h)/(toFloat maxy))
 in
 let width = ceiling (sqsize * (toFloat maxx))
     height = ceiling (sqsize * (toFloat maxy))
 in
 --create collage of appropriate size, not doing anything intelligent here yet and fill it with squares
 Debug.log (toString (sqsize,w,h,maxx,maxy, width, height))
 C.collage 
     width height
     (List.concat (gridMapExtra makesquare (sqsize, width, height) g))
-- else
  --editing mode stuff

fromJust : Maybe a -> a
fromJust a = case a of 
 Just a -> a
 Nothing -> Debug.crash "fromJust we had nothing"

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


tickSignal : Signal Event
tickSignal = (Signal.map (\x-> Tick) (Time.every (Time.millisecond*500)))

eventSignal : Signal Event
eventSignal = (Signal.merge tickSignal (Signal.map (\(x,y)-> Click (x,y)) lastClicked.signal))

upstate : Event -> Grid -> Grid
upstate t g = 
    case t of
        Tick -> updateGrid [0,1,4,5,6,7,8] [3] g 
        Click (x,y) -> Debug.log (toString (x,y)) g

main : Signal E.Element
main = Signal.map2 view Window.dimensions
 (Signal.foldp upstate blinker eventSignal)