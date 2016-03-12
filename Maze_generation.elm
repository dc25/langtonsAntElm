import Maybe as M
import Result as R
import String exposing (toInt)
import Matrix 
import Mouse
import Random exposing (Seed)
import Matrix.Random
import Time exposing (every, second)
import Set exposing (..)
import List exposing (..)
import String exposing (join)
import Html exposing (Html, br, input, h1, h2, text, div, button, fromElement)
import Html.Events exposing (on, targetValue, targetChecked, onClick)
import Html.Attributes as HA
import Svg 
import Svg.Attributes exposing (version, viewBox, cx, cy, r, x, y, x1, y1, x2, y2, fill,points, style, width, height, preserveAspectRatio)

w = 700
h = 700
dt = 0.001

type alias Direction = Int
down = 0
right = 1

type alias Door = (Matrix.Location, Direction)

type State = Initial | Generating | Generated | Solved

type alias Model =
  { rows : Int
  , cols : Int
  , animate : Bool
  , boxes : Matrix.Matrix Bool
  , doors : Set Door
  , current : List Matrix.Location
  , state : State
  , seedStarter : Int
  , seed : Seed
  }

initdoors : Int -> Int -> Set Door
initdoors rows cols =
  let 
    pairs la lb = List.concatMap (\at -> List.map ((,) at) lb) la
    downs = pairs (pairs [0..rows-2] [0..cols-1]) [down] 
    rights = pairs (pairs [0..rows-1] [0..cols-2]) [right] 
  in downs ++ rights |> fromList

init : Int -> Int -> Bool -> State -> Int -> Model
init rows cols animate state starter = 
  let rowGenerator = Random.int 0 (rows-1)
      colGenerator = Random.int 0 (cols-1)
      locationGenerator = Random.pair rowGenerator colGenerator
      (c, s)= Random.generate locationGenerator (Random.initialSeed starter)
  in { rows = rows
     , cols = cols 
     , animate = animate
     , boxes = Matrix.matrix rows cols (\location -> state == Generating && location == c)
     , doors = initdoors rows cols
     , current = if state == Generating then [c] else []
     , state = state
     , seedStarter = starter -- updated every Tick until maze generated.
     , seed = s
     }

view address model =
  let
    greenLineStyle = style "stroke:green;stroke-width:0.3"
    redLineStyle = style "stroke:red;stroke-width:0.1" 

    x1Min = x1 <| toString 0
    y1Min = y1 <| toString 0
    x1Max = x1 <| toString model.cols
    y1Max = y1 <| toString model.rows
    x2Min = x2 <| toString 0
    y2Min = y2 <| toString 0
    x2Max = x2 <| toString model.cols
    y2Max = y2 <| toString model.rows

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

    maze = 
      if model.animate || model.state == Generated || model.state == Initial 
      then [ Svg.g [] <| doors ++ borders ++ unvisited ++ current ] 
      else [ Svg.g [] <| borders ]
  in
    div 
      []
      [ h2 [centerTitle] [text "Maze Generator"]
      , div 
          [floatLeft] 
          [ input
              [ HA.placeholder "rows"
              , let showString = if model.rows >= 10 then model.rows |> toString else ""
                in HA.value showString
              , on "input" targetValue (Signal.message address << SetRows)
              , HA.disabled False
              , HA.style [ ("height", "20px") ]
              , HA.type' "range"
              , HA.min <| toString 10
              , HA.max <| toString 40
              ]
              []
          , "rows=" ++ (model.rows |> toString) |> text
          , br [] []

          , input
              [ HA.placeholder "cols"
              , let showString = if model.cols >= 10 then model.cols |> toString else ""
                in HA.value showString
              , on "input" targetValue (Signal.message address << SetCols)
              , HA.disabled False
              , HA.style [ ("height", "20px") ]
              , HA.type' "range"
              , HA.min <| toString 10
              , HA.max <| toString 40
              ]
              []
          , "cols=" ++ (model.cols |> toString) |> text
          , br [] []

          , input
              [ HA.type' "checkbox"
              , HA.checked model.animate
              , on "change" targetChecked (Signal.message address << SetAnimate)
              ]
              []
          , text "Animate"
          , br [] []
          , button -- start/stop toggle button.
              [ onClick address Generate ]
              [ text "Generate"] 
          ]
      , div 
          [floatLeft] 
          [ Svg.svg 
              [ version "1.1"
              , width (toString w)
              , height (toString h)
              , viewBox (join " " 
                           [ 0 |> toString
                           , 0 |> toString
                           , model.cols |> toString
                           , model.rows |> toString ])
              ] 
              maze
          ]
      ] 

floatLeft = HA.style [ ("float", "left") ] 
centerTitle = HA.style [ ( "text-align", "center") ] 

unvisitedNeighbors : Model -> Matrix.Location -> List Matrix.Location
unvisitedNeighbors model (row,col) = 
  [(row, col-1), (row-1, col), (row, col+1), (row+1, col)]
    |> List.filter (\l -> fst l >= 0 && snd l >= 0 && fst l < model.rows && snd l < model.cols)
    |> List.filter (\l -> (Matrix.get l model.boxes) |> M.withDefault False |> not)

update' : Model -> Int -> Model
update' model t = 
  case head model.current of
    Nothing -> {model | state = Generated, seedStarter = t }
    Just prev ->
      let neighbors = unvisitedNeighbors model prev
      in if (length neighbors) > 0 then
           let (neighborIndex, seed) = Random.generate (Random.int 0 (length neighbors-1)) model.seed
               next = head (drop neighborIndex neighbors) |> M.withDefault (0,0) 
               boxes = Matrix.set next True model.boxes 
               dir = if fst prev == fst next then right else down
               doorCell = if (  (dir == down)   && (fst prev < fst next))  
                             || (dir == right ) && (snd prev < snd next) then prev else next
               doors = Set.remove (doorCell, dir) model.doors 
           in {model | boxes=boxes, doors=doors, current=next :: model.current, seed=seed, seedStarter = t}
         else
           let tailCurrent = tail model.current |> M.withDefault [] 
           in update' {model | current = tailCurrent} t

update : Action -> Model -> Model
update action model = 
  let stringToCellCount s =
    let v' = toInt s |> R.withDefault 10
    in if v' < 10 then 10 else v'
  in case action of 
       Tick t -> 
         if (model.state == Generating) then update' model t
         else { model | seedStarter = t } 

       Generate -> 
         init model.rows model.cols model.animate Generating model.seedStarter

       SetRows countString -> 
         init (stringToCellCount countString) model.cols model.animate Initial model.seedStarter

       SetCols countString -> 
         init model.rows (stringToCellCount countString) model.animate Initial model.seedStarter

       SetAnimate b -> 
         { model | animate = b } 

       NoOp -> model 

control = Signal.mailbox NoOp

type Action = NoOp | Tick Int | Generate | SetRows String | SetCols String | SetAnimate Bool

tickSignal = (every (dt * second)) |> Signal.map (\t -> Tick (round t)) 

actionSignal = Signal.mergeMany [tickSignal, control.signal]

modelSignal = Signal.foldp update (init 21 36 False Initial 0) actionSignal

main = Signal.map (view control.address) modelSignal 
