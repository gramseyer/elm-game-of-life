module Main where
import GameOfLife exposing(..)
import PresetStarts exposing(..)
import Graphics.Element as E
import Graphics.Collage as C
import Signal
import Color
import Array
import Time
import Window

--type alias State = (Grid, Bool) --this depends on the state of the game we're in, ie editing versus viewing mode

-- view will generate a grid of squares black -> false, white -> true
view : (Int, Int) -> Grid -> E.Element
view (w,h) g = 
-- if state then --if we are in viewing mode
 let maxx = Array.length g
     maxy = Array.length (fromJust (Array.get 0 g))
 in 
--the size of the square should be the smallest of the window/sizeofarray
 let sqsize = Basics.min ((toFloat w)/(toFloat maxx)) ((toFloat h)/(toFloat maxy))
 in
 --create collage of appropriate size, not doing anything intelligent here yet and fill it with squares
 C.collage 600 600 (List.concat (gridMapExtra makesquare 60 g))
-- else
  --editing mode stuff

fromJust : Maybe a -> a
fromJust a = case a of
 Just a -> a
 Nothing -> Debug.crash "fromJust we had nothing"

makesquare : ((Int, Int), Bool, Float) -> C.Form
makesquare ((x,y), v, size)= 
 if v then
  C.move ((toFloat x) *size, (toFloat y) *size) (C.filled Color.white (C.square size)) 
 else 
  C.move (((toFloat x) *size), (toFloat y) *size) (C.filled Color.black (C.square size)) 

-- List.map (\w-> List.map (\v -> (v, Array.get (snd v) ((Array.get (fst v) grid))))) (List.map (\f->List.map f [0..yMax]) (List.map (\x->(\y->(x,y))) [0..xMax]))

upstate : Time.Time -> Grid -> Grid
upstate t g = Debug.log "something" updateGrid (3,3) [0,1,4,5,6,7,8] [3] g 

main : Signal E.Element
main = Signal.map2 view Window.dimensions
 (Signal.foldp upstate blinker (Time.every (Time.millisecond*500)))