module Main where
import GameOfLife exposing(..)

import Graphics.Element
import Graphics.Collage
import Signal
import Color
import Array

type alias State = (Grid, Bool) --this depends on the state of the game we're in, ie editing versus viewing mode

-- view will generate a grid of squares black -> false, white -> true
view : (Int, Int) -> State -> Element
view (w,h) (g, state) = 
 if state then --if we are in viewing mode
 let maxx = Array.length g
     maxy = Array.length (Basics.fst g)
 in 
--the size of the square should be the smallest of the window/sizeofarray
 let sqsize = Basics.min (w/maxx) (h/maxy)
 in
 --create collage of appropriate size, not doing anything intelligent here yet and fill it with squares
 Graphics.Collage.collage 300 300 (List.concat (gridMap makesquare size g))
 else
  --editing mode stuff

makesquare : ((Int, Int), Bool, Float) -> Form
makesquare ((x,y), v, size)= 
 if v then
  move (x*size, y*size) (Graphics.Collage.filled Color.white (Graphics.Collage.square size)) 
 else 
  move (x*size, y*size) (Graphics.Collage.filled Color.black (Graphics.Collage.square size)) 

-- List.map (\w-> List.map (\v -> (v, Array.get (snd v) ((Array.get (fst v) grid))))) (List.map (\f->List.map f [0..yMax]) (List.map (\x->(\y->(x,y))) [0..xMax]))


main : Signal Element
main grid= Signal.map2 view Window.dimensions
 (Signal.foldp upstate initState grid)