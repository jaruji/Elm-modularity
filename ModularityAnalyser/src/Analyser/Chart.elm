module Analyser.Chart exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Chart as C exposing (..)
import Chart.Attributes as CA exposing (..)
import Chart.Events as CE exposing (..)
import Chart.Svg as CS
import Svg as S
import Dict exposing (Dict)
import Analyser.Metrics.Metric exposing (Value)
import List exposing (map, filter, drop)
import RadarChart

--this module contains all the chart logic. It has no state nor any functionality except visualizing data through Html messages.
--It uses the elm-charts library

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

viewRadarChart: Html msg
viewRadarChart =
  div[ style "height" "500px", style "width" "500px" ][
    RadarChart.view RadarChart.defaultOptions 
      [ "Values", "Variables", "Conditionals", "Loops", "Functions", "Programs" ]
      [ { color = "yellow", data = [ 120, 500, 310, 130, 300, 180 ] } ]
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