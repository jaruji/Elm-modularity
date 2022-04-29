module Analyser.Chart exposing (..)
import Html exposing (Html)
import Chart as C
import Chart.Attributes as CA

view : Html msg
view =
  C.chart
    [ CA.width 300
    , CA.height 300
    ]
    [ C.xTicks []
    , C.yTicks []
    , C.xLabels []
    , C.yLabels []
    , C.bars []
        [ C.bar .income [ CA.color "red" ]
        , C.bar .spending [ CA.opacity 0.8 ]
        ]
        [{income = 25, spending = 80}, {income = 35, spending = 10}, {income = 15, spending = 155}]
    ]