import Matrix 
import Random exposing (Seed)
import Matrix.Random
import Set exposing (..)
import List exposing (..)
import String exposing (join)
import Html exposing (Html, br, input, h1, h2, text, div, button, fromElement)
import Html.Attributes as HA
import Svg 
import Svg.Attributes exposing (version, viewBox, cx, cy, r, x, y, x1, y1, x2, y2, fill,points, transform, style, width, height)

w = 500
h = 700

rows = 30
cols = 40

type alias Box = 
  { visited : Bool
  }

type alias Direction = Int
up = 0
down = 1
left = 2
right = 3

type alias Wall = (Matrix.Location, Direction)

type alias Model =
  { boxes : Matrix.Matrix Box
  , walls : Set Wall
  , current : Matrix.Location
  , seed : Seed
  }

initWalls : Int -> Int -> Set Wall
initWalls rows cols =
  let downAndRight = map3 (\r c d -> ((r,c), d)) [0..rows-2] [0..cols-2] [down, right] 
      onlyDown = map3 (\r c d -> ((r,c), d)) [0..rows-2] [cols-1..cols-1] [down] 
      onlyRight = map3 (\r c d -> ((r,c), d)) [rows-1..rows-1] [0..cols-2] [right] 
  in downAndRight ++ onlyDown ++ onlyRight |> fromList


init : Model
init = 
  let rowGenerator = Random.int 0 (rows-1)
      colGenerator = Random.int 0 (cols-1)
      locationGenerator = Random.pair rowGenerator colGenerator
      (c, s)= Random.generate locationGenerator (Random.initialSeed 45)
  in { boxes = Matrix.matrix rows cols (\location -> { visited = False }) 
     , walls = initWalls rows cols
     , current = c
     , seed = s
     }

view model =
  let
    svgPendulum = 
      Svg.g 
        [ ]
        [ Svg.line [ y1 "0"
                   , y2 "100"
                   , style "stroke:red;stroke-width:2" ] []
        ]

  in
    div []
      [ div floatLeft [ h2 centerTitle [text "SVG"]
                      , Svg.svg -- svg element to hold pendulum
                          [ version "1.1"
                          , width (toString w)
                          , height (toString h)
                          , viewBox (join " " 
                                       [-w//2 |> toString
                                       ,-h//2 |> toString
                                       ,    w |> toString
                                       ,    h |> toString ])
                          ] 
                          [ svgPendulum ]
                      ]

      ] 


floatLeft = [ HA.style [ ("float", "left") ] ]
centerTitle = [ HA.style [ ( "text-align", "center") ] ]

main = view init
