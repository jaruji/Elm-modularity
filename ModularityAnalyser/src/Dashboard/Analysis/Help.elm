module Dashboard.Analysis.Help exposing (..)

import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Analyser.File exposing (File_)
import Analyser.Metrics.Metric exposing (Metric, Value)
import Dict exposing (Dict, values)
import Debug exposing (toString)

type alias Model = 
    {
        metrics: Dict String Metric,
        search: String
    }

type Msg
    = NoOp
    | UpdateSearch String


init: Dict String Metric -> ( Model, Cmd Msg)
init metrics =
    ({ metrics = metrics, search = "" }, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        UpdateSearch search ->
            ({ model | search = search }, Cmd.none)

view: Model -> Html Msg
view model =
    div[][
        div[ class "header" ][
            h1[][ text "Help"],
            div[ class "subtext" ][ text "Basic information about used metrics, what they measure and how were the threshold values obtained."]
        ],
        div[ class "main-header" ][],
        div[ class "explanation" ][
            input [ id "search", value model.search, placeholder "Search...", onInput UpdateSearch ] []
        ],
        div[] <| List.map viewMetricInfo (List.filter (\val ->
            if String.contains (String.toLower model.search) (String.toLower val.name) then
                True
            else
                False
        ) (values model.metrics) )
    ]

viewMetricInfo: Metric -> Html msg 
viewMetricInfo metric =
    div[ class "card", style "width" "100%" ][
        h2[][ text metric.name ],
        h3[][ text "Description"],
        hr[ style "width" "20%" ][],
        div[ class "explanation" ][
            text metric.description
        ],
        h3[][ text "Thresholds"],
        hr[ style "width" "20%" ][],
        table[ style "width" "30%", style "text-align" "left" ][
            tr[][
                th[][ text "Lower threshold"],
                th[][ text "Upper threshold"]
            ],
            tr[][
                td[][ text (metric.lowerThreshold |> toString)],
                td[][ text (metric.upperThreshold |> toString)]
            ]
        ]
    ]

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model