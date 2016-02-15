module Main where
import GameOfLife exposing(..)

import Graphics.Element as E
import Graphics.Collage as C
import Signal
import Color
import Array

--type alias State = (Grid, Bool) --this depends on the state of the game we're in, ie editing versus viewing mode

-- view will generate a grid of squares black -> false, white -> true
view : (Int, Int) -> Grid -> E.Element
view (w,h) g = 
-- if state then --if we are in viewing mode
-- let maxx = Array.length g
--     maxy = Array.length (Basics.fst g)
-- in 
--the size of the square should be the smallest of the window/sizeofarray
-- let sqsize = Basics.min (w/maxx) (h/maxy)
-- in
 --create collage of appropriate size, not doing anything intelligent here yet and fill it with squares
 C.collage 300 300 (List.concat (gridMapExtra makesquare 4 g))
-- else
  --editing mode stuff

makesquare : ((Int, Int), Bool, Float) -> C.Form
makesquare ((x,y), v, size)= 
 if v then
  C.move ((toFloat x) *size, (toFloat y) *size) (C.filled Color.white (C.square size)) 
 else 
  C.move ((toFloat x) *size, (toFloat y) *size) (C.filled Color.black (C.square size)) 

-- List.map (\w-> List.map (\v -> (v, Array.get (snd v) ((Array.get (fst v) grid))))) (List.map (\f->List.map f [0..yMax]) (List.map (\x->(\y->(x,y))) [0..xMax]))


--main : Signal Element
--main grid= Signal.map2 view Window.dimensions
 --(Signal.foldp upstate initState grid)