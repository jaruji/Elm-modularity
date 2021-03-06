module Analyser.Chart exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Chart as C exposing (..)
import Chart.Attributes as CA exposing (..)
import Chart.Events as CE exposing (..)
import Chart.Svg as CS
import Chart.Item as CI
import Svg as S
import Svg.Attributes exposing (viewBox)
import Dict exposing (Dict)
import Analyser.Metrics.Metric exposing (Value)
import List exposing (map, filter, drop)
import RadarChart

--It uses the elm-charts library

type alias Model =
  --state to execute the hover over heatmap functionality -> popup with tooltip displayed over every node
  { hovering : List (CI.One { x : Float, y : Float } CI.Any) }


init : Model
init =
  { hovering = [] }


type Msg
  = OnHover (List (CI.One { x : Float, y : Float } CI.Any))

update : Msg -> Model -> Model
update msg model =
  case msg of
    OnHover hovering ->
      { model | hovering = hovering }

viewHeatmap: Model -> List (List Int) -> List String -> Html Msg
viewHeatmap model matrix filenames =
  C.chart
  [ 
    CA.height 1000,
    CA.width 1000,
    CE.onMouseMove OnHover (CE.getNearest CI.any),
    CE.onMouseLeave (OnHover [])
    --CA.attrs [ viewBox "0 0 1000 1000"]
  ][ 
    C.xTicks [ CA.noGrid, CA.amount (List.length matrix) ],
  --, (List.map(\val -> C.xLabel[ CA.rotate 80 ][ S.text val ] ) filenames)
    -- C.generate (List.length matrix) C.ints .x [] <| \plane int -> [
    --   C.xLabel[ CA.x ((toFloat (remainderBy (List.length matrix) int) * 0.4) + 0.4), CA.moveRight 1, CA.rotate 90 ][ S.text "Test" ],
    --   C.yLabel[ CA.y ((toFloat (remainderBy (List.length matrix) int) * 0.4) + 0.4), CA.moveUp 5 ][ S.text "Test" ]
    -- ],
  --, C.xLabel [][ S.text (Debug.toString filenames) ]
    C.yTicks [ CA.noGrid, CA.amount (List.length matrix) ],
    --C.yLabels [],
    C.list <|
      let heatmapItem index value =
            let x = toFloat (remainderBy (List.length matrix) index) * 0.2
                y = toFloat (index // (List.length matrix)) * 0.2
                color =
                  if value > 15 then "magenta" else
                  if value > 10 then "red" else
                  if value > 6  then "orange" else
                  if value > 4  then "yellow" else
                  if value > 1  then "brown"
                  else
                    "black"
            in
              C.custom
                { name = "",
                  color = "black",
                  position = { x1 = x, x2 = x + 0.2, y1 = y, y2 = y + 0.2 },
                  format = .y >> String.fromFloat >> (\v -> v ++ " calls"),
                  data = { x = toFloat index, y = value },
                  render = \p ->
                    CS.rect p
                      [ CA.x1 x
                      , CA.x2 (x + 0.2)
                      , CA.y1 y
                      , CA.y2 (y + 0.2)
                      , CA.color color
                      , CA.border "white"
                      ]
                }
      in
        let 
          modifiedMatrix =
            --adjacency matrix with real number of calls replacing the simple 1s (as in modules are neighbors)
            List.foldl(\val acc -> 
              acc 
              ++
              List.map(\value -> 
                value |> toFloat
              ) val
            ) [] matrix
          in
            List.indexedMap heatmapItem modifiedMatrix,       
    C.each model.hovering <| \_ item ->
      [ C.tooltip item [ CA.center, CA.offset 0, CA.onTopOrBottom ] [] [] ]
  ]

viewMetricBarplot : String -> Float -> Float -> Float -> List Value -> Html msg
viewMetricBarplot name avg lower upper metrics =
  --histogram, shows the distribution of metric values in all modules.
  --has 3 additional lines over the histogram - average value, upper and lower thresholds
  let
      values = 
        List.map(\val -> 
          let 
            split = String.split "." val.parentDeclaration
          in
            if List.length split > 2 then
              { val | parentDeclaration = String.join "." (List.drop 1 split)}
            else
              val
        ) metrics
  in
    C.chart
      [ 
        height 800,
        width 1000
      ]
      [ C.xTicks []
      , C.yTicks []
      , C.binLabels .parentDeclaration [ CA.moveDown 100, CA.rotate 80 ]
      , C.yLabels []
      
      , C.bars [ CA.roundTop 0.5 ]
          [ 
            
            C.bar .value [ CA.gradient [ CA.blue, CA.darkBlue ]]
          ]
          values,
        C.withPlane <| \p ->
        [ 
          --draw the 3 lines
          C.line[ 
            CA.x1 p.x.min,
            CA.y1 avg,
            CA.x2 p.x.max,
            CA.dashed [ 10, 10 ],
            CA.color CA.green,
            CA.width 5
          ],
          C.line[ 
            CA.x1 p.x.min,
            CA.y1 lower,
            CA.x2 p.x.max,
            CA.dashed [ 10, 10 ],
            CA.color CA.red,
            CA.width 5
          ],
          C.line[ 
            CA.x1 p.x.min,
            CA.y1 upper,
            CA.x2 p.x.max,
            CA.dashed [ 10, 10 ],
            CA.color CA.red,
            CA.width 5
          ]
        ],
        C.labelAt CA.middle .max [ CA.fontSize 30, CA.moveUp 15 ]
          [ S.text name ]
      ]