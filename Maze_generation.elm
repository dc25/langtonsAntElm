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
import Svg.Attributes exposing (version, viewBox, cx, cy, r, x, y, x1, y1, x2, y2, fill,points, style, width, height, preserveAspectRatio)

w = 700
h = 700
rows = 25
cols = 25
dt = 0.001

type alias Box = Bool

type alias Direction = Int
down = 0
right = 1

type alias Door = (Matrix.Location, Direction)

type alias Model =
  { boxes : Matrix.Matrix Box
  , doors : Set Door
  , current : List Matrix.Location
  , seed : Seed
  }

initdoors : Int -> Int -> Set Door
initdoors rows cols =
  let 
    pairs la lb = List.concatMap (\at -> List.map ((,) at) lb) la
    downs = pairs (pairs [0..rows-2] [0..cols-1]) [down] 
    rights = pairs (pairs [0..rows-1] [0..cols-2]) [right] 
  in downs ++ rights |> fromList

init : Model
init = 
  let rowGenerator = Random.int 0 (rows-1)
      colGenerator = Random.int 0 (cols-1)
      locationGenerator = Random.pair rowGenerator colGenerator
      (c, s)= Random.generate locationGenerator (Random.initialSeed 45)
  in { boxes = Matrix.matrix rows cols (\location -> location == c ) 
     , doors = initdoors rows cols
     , current = [c]
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
      let (deltaX1, deltaY1) = if (snd door == right) then (1,0) else (0,1)
          (row, column) = fst door
      in Svg.line [ x1 <| toString (column + deltaX1)
                  , y1 <| toString (row    + deltaY1)
                  , x2 <| toString (column + 1)
                  , y2 <| toString (row    + 1)
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

    current = 
      case head model.current of
          Nothing -> []
          Just c -> [circleInBox c "black"]

    maze = Svg.g [] <| doors ++ borders ++ unvisited ++ current
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
  [(row, col-1), (row-1, col), (row, col+1), (row+1, col)]
    |> List.filter (\l -> fst l >= 0 && snd l >= 0 && fst l < rows && snd l < cols)
    |> List.filter (\l -> (Matrix.get l model.boxes) |> withDefault False |> not)

update' : Model -> List Matrix.Location -> Model
update' model current = 
  case head current of
    Nothing -> model
    Just previous ->
      let neighbors = unvisitedNeighbors model previous
      in if (length neighbors) > 0 then
           let (neighborIndex, seed) = Random.generate (Random.int 0 (length neighbors-1)) model.seed
               nextBox = head (drop neighborIndex neighbors) |> withDefault (0,0) 
               boxes = Matrix.set nextBox True model.boxes 
               direction = if fst previous == fst nextBox then right else down
               doorCell = 
                 if (direction == down) then 
                   if (fst previous < fst nextBox) then previous else nextBox
                 else
                   if (snd previous < snd nextBox) then previous else nextBox
               doors = Set.remove (doorCell, direction) model.doors 
           in {boxes=boxes, doors=doors, current=nextBox :: model.current, seed=seed}
         else
           tail current |> withDefault [] |> update' model

update : Action -> Model -> Model
update action model = update' model model.current 

type Action = Tick 

tickSignal = (every (dt * second)) |> Signal.map (always Tick)

modelSignal = Signal.foldp update init tickSignal

main = Signal.map view modelSignal 
