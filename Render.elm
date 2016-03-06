module Render (view, renderBoxList, renderNewGridInputFields, renderMaxGridInputFields) where

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
import String
import Text
import Window

-- view will generate a grid of squares black -> false, white -> true
view : Signal.Mailbox Event -> Signal.Mailbox ClickEvent -> (Int, Int) -> State -> E.Element -> E.Element ->E.Element -> E.Element -> E.Element
view statechange lastClicked (w,h) state newGridFields liveToDeathChecks deadToLifeChecks maxGridFields = 
  let renderedUIElements = renderButtons statechange (w,h) state in
  let uiElements = E.flow E.right (List.append renderedUIElements [newGridFields, (renderGameControlPanel deadToLifeChecks liveToDeathChecks), maxGridFields, (renderDropdown statechange state)]) in
    E.flow E.down ((renderGrid lastClicked (w,h) state) :: [uiElements])

renderGrid : Signal.Mailbox ClickEvent -> (Int, Int) -> State -> E.Element
renderGrid lastClicked (w,h) state = 
  let (sqsize, width, height) = findsqsizewh (w,h) state.g
  in 
  --create collage of appropriate size, not doing anything intelligent here yet and fill it with squares
  C.collage 
    width height
    ((C.filled Color.blue (C.rect (toFloat(width)) (toFloat(height)))) :: (List.concat (gridMapExtra (makesquare lastClicked) (sqsize, width, height) state.g)))

findsqsizewh : (Int, Int) -> Grid -> (Float, Int, Int)
findsqsizewh (w,h) g = 
  let (maxx, maxy) = getDimensions g in 
--the size of the square should be the smallest of the window/sizeofarray
  let sqsize = Basics.min ((toFloat w)/(toFloat maxx)) ((toFloat (h - 100))/(toFloat maxy)) in 
  let width = ceiling (sqsize * (toFloat maxx))
      height = ceiling (sqsize * (toFloat maxy))
  in
    (sqsize, width, height)

makeSquareForm : Color.Color -> Float -> C.Form
makeSquareForm color size = (C.group [(C.filled color (C.square size)), (C.outlined {defaultLine | color = Color.blue} (C.square size)) ]) 

makeFormIntoClickable : Signal.Mailbox ClickEvent -> Int -> Int -> Int ->C.Form -> C.Form
makeFormIntoClickable lastClicked x y size form=
  C.toForm (I.clickable (Signal.message lastClicked.address (x,y)) (C.collage size size [form]))

makesquare : Signal.Mailbox ClickEvent -> ((Int, Int), Bool, (Float, Int, Int)) -> C.Form
makesquare lastClicked ((x,y), v, (size, maxx, maxy)) = 
  let coords = ((0.5+(toFloat x)) *size - ((toFloat maxx)/2) , (0.5+ (toFloat y)) *size - ((toFloat maxy)/2)) in
  if v then
    C.move coords (makeFormIntoClickable lastClicked x y (round size) (makeSquareForm Color.white size))
  else 
    C.move coords (makeFormIntoClickable lastClicked x y (round size) (makeSquareForm Color.black size))

renderGameControlPanel : E.Element -> E.Element -> E.Element
renderGameControlPanel deadToLife liveToDeath =
  let labels = E.flow E.right (List.map (\x-> E.container 40 40 E.middle (E.show x)) [0..8])
      names = E.flow E.down [renderText "Neighbors", renderText "liveToDeath", renderText "deadToLife"]
  in 
  E.flow E.right [  names
                  , E.flow E.down [ labels
                                  , liveToDeath
                                  , deadToLife
                                  ]
                ]

renderBoxList : List (Signal.Mailbox Bool) -> Signal E.Element
renderBoxList checkBoxes = let listOfSignals = (List.map ((\x -> Signal.map (renderBox x) x.signal)) checkBoxes) in
  recursiveMerge listOfSignals

renderBox : Signal.Mailbox Bool -> Bool -> E.Element
renderBox check bool = E.container 40 40 E.middle (I.checkbox (Signal.message check.address) bool)

recursiveMerge : List (Signal E.Element) -> Signal E.Element
recursiveMerge signals = case signals of 
  x::[] -> x
  x::xs -> mergeTwoBoxes x (recursiveMerge xs)
  [] -> Debug.crash "recursiveMerge"

mergeTwoBoxes : Signal E.Element -> Signal E.Element -> Signal E.Element
mergeTwoBoxes x y = Signal.map2 (\x->\y-> E.flow E.right [x,y]) x y

renderText : String -> E.Element
renderText str = E.container 100 40 E.middle (E.centered (Text.fromString str))

renderButtons : Signal.Mailbox Event -> (Int, Int) -> State -> List E.Element
renderButtons statechange (w,h) state = 
    [ (I.button (Signal.message statechange.address changeMode) (getToggleButtonText state))
    , (I.button (Signal.message statechange.address clearGrid) "Clear Grid")
    , (I.button (Signal.message statechange.address newGrid) "New Grid") 
    ]

getToggleButtonText : State -> String
getToggleButtonText state = if state.running then "Stop Simulation" else "Start Simulation"

renderNewGridInputFields : Signal.Mailbox F.Content -> Signal.Mailbox F.Content -> F.Content -> F.Content -> E.Element
renderNewGridInputFields fieldX fieldY contentx contenty = 
  let renderedXField = renderField fieldX "New X-Dimension" contentx in
  let renderedYField = renderField fieldY "New Y-Dimension" contenty in
  E.flow E.down [renderedXField, renderedYField]

renderMaxGridInputFields : Signal.Mailbox F.Content -> Signal.Mailbox F.Content -> F.Content -> F.Content -> E.Element
renderMaxGridInputFields fieldX fieldY contentX contentY = 
  let renderedXField = renderField fieldX "Max X-Dimension" contentX in
  let renderedYField = renderField fieldY "Max Y-Dimension" contentY in
  E.flow E.down [renderedXField, renderedYField]

renderField : Signal.Mailbox F.Content -> String -> F.Content -> E.Element
renderField field defaultString = F.field F.defaultStyle (Signal.message field.address) defaultString

renderDropdown : Signal.Mailbox Event -> State -> E.Element
renderDropdown statechange state = 
  I.dropDown (Signal.message statechange.address) 
    (("",        identity) ::
      (List.map (\(str, grid) -> (str, (loadGridUpdate str))) state.savedGrids))
