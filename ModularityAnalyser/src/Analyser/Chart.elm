module Analyser.Chart exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Chart as C exposing (..)
import Chart.Attributes as CA exposing (..)
import Chart.Events as CE exposing (..)
import Chart.Svg as CS
import Chart.Item as CI
import Svg as S
import Dict exposing (Dict)
import Analyser.Metrics.Metric exposing (Value)
import List exposing (map, filter, drop)
import RadarChart

--this module contains all the chart logic. It has no state nor any functionality except visualizing data through Html messages.
--It uses the elm-charts library

type alias Model =
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

viewHeatmap: Model -> Html Msg
viewHeatmap model =
  C.chart
  [ CA.height 300
  , CA.width 300
  , CE.onMouseMove OnHover (CE.getNearest CI.any)
  , CE.onMouseLeave (OnHover [])
  ]
  [ C.xTicks []
  , C.xLabels []
  , C.yTicks []
  , C.yLabels []
  , C.list <|
      let heatmapItem index value =
            let x = toFloat (remainderBy 5 index) * 2
                y = toFloat (index // 5) * 2
                color =
                  if value > 5 then "darkred" else
                  if value > 3  then "red" else
                  if value > 1  then "yellow" else
                  if value == 1  then "lightgreen"
                  else
                    "white"
            in
            C.custom
              { name = "Modularity heatmap"
              , color = color
              , position = { x1 = x, x2 = x + 2, y1 = y, y2 = y + 2 }
              , format = .y >> String.fromFloat >> (\v -> v ++ " C°")
              , data = { x = toFloat index, y = value }
              , render = \p ->
                  CS.rect p
                    [ CA.x1 x
                    , CA.x2 (x + 2)
                    , CA.y1 y
                    , CA.y2 (y + 2)
                    , CA.color color
                    , CA.border "white"
                    ]
              }
      in
      List.indexedMap heatmapItem
        [ 2, 5, 8, 5, 3
        , 5, 7, 9, 0, 3
        , 2, 4, 6, 3, 5
        , 7, 9, 0, 3, 2
        , 4, 6, 7, 8, 10
        ]
        
  , C.each model.hovering <| \_ item ->
      [ C.tooltip item [ CA.center, CA.offset 0, CA.onTopOrBottom ] [] [] ]
  ]

viewMetricBarplot : String -> Float -> List Value -> Html msg
viewMetricBarplot name avg metrics =
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
          C.line[ 
            CA.x1 p.x.min,
            CA.y1 avg,
            CA.x2 p.x.max,
            CA.dashed [ 10, 10 ],
            CA.color CA.red,
            CA.width 5
          ]
        ],
        C.labelAt CA.middle .max [ CA.fontSize 30, CA.moveUp 15 ]
          [ S.text name ]
      ]

viewTemplateGraph : Html msg
viewTemplateGraph =
  C.chart
    [ height 325
    , width 600
    ]
    [ C.xTicks []
    , C.yTicks []
    , C.binLabels .label [ CA.moveDown 20 ]
    , C.yLabels []
    , C.bars []
        [ 
          C.bar .income [ CA.color "pink" ]
        ]
        [{income = 1.0, label= "test1"},{income =5, label = "test2"}]
    ]