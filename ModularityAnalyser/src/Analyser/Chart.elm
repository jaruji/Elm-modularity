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
import List exposing (map)
import RadarChart

--this module contains all the chart logic. It has no state nor any functionality except visualizing data through Html messages.
--It uses the elm-charts library

viewMetricBarplot : String -> Float -> List Value -> Html msg
viewMetricBarplot name avg metrics =
  C.chart
    [ 
      height 600,
      width 600
    ]
    [ C.xTicks []
    , C.yTicks []
    , C.binLabels .parentDeclaration [ CA.moveDown 80, CA.rotate 80 ]
    , C.yLabels []
    
    , C.bars []
        [ 
          C.bar .value [ CA.color "pink" ],
          C.bar .value [ CA.striped [ CA.spacing 4 ]]
        ]
        metrics,
      C.withPlane <| \p ->
      [ 
        C.line[ 
          CA.x1 p.x.min,
          CA.y1 avg,
          CA.x2 p.x.max,
          CA.dashed [ 10, 10 ],
          CA.color CA.red
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