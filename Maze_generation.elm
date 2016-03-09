import Maybe exposing (withDefault)
import Matrix 
import Random exposing (Seed)
import Matrix.Random
import Time exposing (every, second)
import Set exposing (..)
import List exposing (..)
import String exposing (join)
import Html exposing (Html, br, input, h1, h2, text, div, button, fromElement)
import Html.Attributes as HA
import Svg 
import Svg.Attributes exposing (version, viewBox, cx, cy, r, x, y, x1, y1, x2, y2, fill,points, transform, style, width, height, preserveAspectRatio)

w = 700
h = 700
dt = 0.01

rows = 30
cols = 30

type alias Box = Bool

type alias Direction = Int
up = 0
down = 1
left = 2
right = 3

type alias Door = (Matrix.Location, Direction)

type alias Model =
  { boxes : Matrix.Matrix Box
  , doors : Set Door
  , current : Matrix.Location
  , seed : Seed
  }

-- is there some better (maybe built in?) way of doing this?
pairs : List a -> List b -> List (a,b)
pairs la lb = List.concatMap (\at -> List.map ((,) at) lb) la

initdoors : Int -> Int -> Set Door
initdoors rows cols =
  let 
    downAndRight = pairs (pairs [0..rows-2] [0..cols-2]) [down, right] 
    onlyDown = pairs (pairs [0..rows-2] [cols-1]) [down] 
    onlyRight = pairs (pairs [rows-1] [0..cols-2]) [right] 
  in downAndRight ++ onlyDown ++ onlyRight |> fromList

init : Model
init = 
  let rowGenerator = Random.int 0 (rows-1)
      colGenerator = Random.int 0 (cols-1)
      locationGenerator = Random.pair rowGenerator colGenerator
      (c, s)= Random.generate locationGenerator (Random.initialSeed 45)
  in { boxes = Matrix.matrix rows cols (\location -> location == c ) 
     , doors = initdoors rows cols
     , current = c
     , seed = s
     }

view model =
  let
    greenLineStyle = style "stroke:green;stroke-width:0.3"
    redLineStyle = style "stroke:red;stroke-width:0.1" 

    x1Min = x1 <| toString 0
    y1Min = y1 <| toString 0
    x1Max = x1 <| toString cols
    y1Max = y1 <| toString rows
    x2Min = x2 <| toString 0
    y2Min = y2 <| toString 0
    x2Max = x2 <| toString cols
    y2Max = y2 <| toString rows

    borders = [ Svg.line [ x1Min, y1Min, x2Max, y2Min, greenLineStyle ] []
              , Svg.line [ x1Max, y1Min, x2Max, y2Max, greenLineStyle ] []
              , Svg.line [ x1Max, y1Max, x2Min, y2Max, greenLineStyle ] []
              , Svg.line [ x1Min, y1Max, x2Min, y2Min, greenLineStyle ] []
              ]

    doorToLine door = 
      let side = snd door
          (deltaX1, deltaY1) = if (side == right) then (1,0) else (0,1)
          (row, column) = fst door
          x1value = column + deltaX1
          x2value = column + 1
          y1value = row    + deltaY1
          y2value = row    + 1
      in Svg.line [ x1 <| toString x1value
                  , y1 <| toString y1value
                  , x2 <| toString x2value
                  , y2 <| toString y2value 
                  , redLineStyle ] []

    doors = (List.map doorToLine <| toList model.doors )

    circleInBox (row,col) color = 
      Svg.circle [ r "0.25"
      , fill (color)
      , cx (toString (toFloat col + 0.5))
      , cy (toString (toFloat row + 0.5))
      ] [] 

    showUnvisited location box =
       if box then [] else [ circleInBox location "yellow" ]

    unvisited = model.boxes 
                  |> Matrix.mapWithLocation showUnvisited 
                  |> Matrix.flatten 
                  |> concat

    current = [circleInBox model.current "black"]

    maze = 
      Svg.g [] <| doors ++ borders ++ unvisited ++ current
  in
    div []
      [ div floatLeft [ h2 centerTitle [text "Maze Generator"]
                      , Svg.svg 
                          [ version "1.1"
                          , width (toString w)
                          , height (toString h)
                          , viewBox (join " " 
                                       [ 0 |> toString
                                       , 0 |> toString
                                       , cols |> toString
                                       , rows |> toString ])
                          ] 
                          [ maze ]
                      ]

      ] 

floatLeft = [ HA.style [ ("float", "left") ] ]
centerTitle = [ HA.style [ ( "text-align", "center") ] ] 

unvisitedNeighbors : Model -> Matrix.Location -> List Matrix.Location
unvisitedNeighbors model (row,col) = 
 let n0 = Debug.watch "n0" ([(row, col-1), (row-1, col), (row, col+1), (row+1, col)] )
     n1 = Debug.watch "n1" (n0 |> List.filter (\l -> fst l > 0 && snd l > 0 && fst l < rows && snd l < cols))
     n2 = Debug.watch "n2" (n1 |> List.filter (\l -> (Matrix.get l model.boxes) |> withDefault False |> not))
 in n2

update : Action -> Model -> Model
update action model = 
  let neighbors = Debug.watch "neighbors" (unvisitedNeighbors model model.current)
  in 
    if (length neighbors) > 0 then
      let (neighborIndex, seed) = Random.generate (Random.int 0 (length neighbors-1)) model.seed
          previous = Debug.watch "previous" model.current
          current = head (drop neighborIndex neighbors) |> withDefault (0,0) |> Debug.watch "current"
          boxes = Matrix.set current True model.boxes 
          direction = if fst previous == fst current then right else down
          doorCell = Debug.watch "doorCell" <|
            if (direction == down) then 
              if (fst previous < fst current) then previous else current
            else
              if (snd previous < snd current) then previous else current
            
          doors = Set.remove (doorCell, direction) model.doors 
      in {boxes=boxes, doors=doors, current=current, seed=seed}
    else
      model

type Action = Tick 

tickSignal = (every (dt * second)) |> Signal.map (always Tick)

modelSignal = Signal.foldp update init tickSignal

main = Signal.map view modelSignal 
