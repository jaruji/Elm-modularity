module Analyser.Chart exposing (..)
import Html exposing (Html, div)
import Chart as C exposing (..)
import Chart.Attributes as CA exposing (..)
import Chart.Events as CE exposing (..)

--this module contains all the chart logic. It has no state nor any functionality except visualizing data through Html messages.
--It uses the elm-charts library


-- viewBarPlot: List String -> List Int -> Html msg
-- viewBarPlot string int =
--   C.chart
--     [ CA.width 300    -- Sets width dimension of chart
--     , CA.height 500   -- Sets height dimension of chart
--       -- Note that the chart scales with it's container

--     , CA.margin { top = 10, bottom = 20, left = 20, right = 20 }
--                       -- Add space around your chart.
--                       -- Useful if you have labels which extend
--                       -- outside the main chart area.

--     , CA.padding { top = 10, bottom = 10, left = 10, right = 10 }
--                       -- Expand your domain / range by a set
--                       -- amount of SVG units.
--                       -- Useful if you have e.g. scatter dots
--                       -- which extend beyond your main chart area,
--                       -- and you'd like them to be within.

--     -- Control the range and domain of your chart.
--     -- Your range and domain is by default set to the limits of
--     -- your data, but you can change them like this:
--     , CA.range
--         [ CA.lowest -5 CA.orLower
--             -- Makes sure that your x-axis begins at -5 or lower, no matter
--             -- what your data is like.
--         , CA.highest 10 CA.orHigher
--             -- Makes sure that your x-axis ends at 10 or higher, no matter
--             -- what your data is like.
--         ]
--     , CA.domain
--         [ CA.lowest 0 CA.exactly ]
--             -- Makes sure that your y-axis begins at exactly 0, no matter
--             -- what your data is like.

--     -- Add event triggers to your chart. Learn more about these in
--     -- the `Chart.Events` module.
--     , CE.onMouseMove OnHovering (CE.getNearest CI.bars)
--     , CE.onMouseLeave (OnHovering [])
--     --toto naozaj robila sprosta krava, nikdy som nevidel horsi balik jak toto hovno. zabi sa tava
--     -- Add arbitrary HTML and SVG attributes to your chart.
--     , CA.htmlAttrs [ HA.style "background" "beige" ]
--     , CA.attrs [ SA.id "my-chart" ]
--     ]
--     [ C.grid []
--     , C.xLabels []
--     , C.yLabels []
--     ]

viewTemplateGraph : Html msg
viewTemplateGraph =
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