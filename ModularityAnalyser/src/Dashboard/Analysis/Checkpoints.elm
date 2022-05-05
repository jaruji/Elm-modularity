module Dashboard.Analysis.Checkpoints exposing (..)

import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Analyser.Metrics.Metric as Metric exposing (Metric, Value)
import Ports exposing (storeCheckpoint, loadCheckpoints)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (..)

type alias Model = 
    {
        checkpoints: List Checkpoint,
        metrics: Dict String Metric
    }

type alias Checkpoint =
    {
        projectId: String,
        metrics: Dict String Metric,
        timestamp: String
    }

init: Dict String Metric -> ( Model, Cmd Msg )
init metrics =
    ({ checkpoints = [], metrics = metrics }, Cmd.none)

type Msg
    = NoOp
    | Delete --remove checkpoints from Local Storage
    | SetCheckpoint -- store checkpoint into Local Storage

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        Delete ->
            (model, Cmd.none)
        SetCheckpoint ->
            (model, storeCheckpoint (dictToJson model.metrics))

dictToJson: Dict String Metric -> Encode.Value
dictToJson metrics =
    Encode.dict keyToString encodeMetric metrics

keyToString: String -> String
keyToString key =
    key

encodeMetric: Metric -> Encode.Value
encodeMetric metric =

    -- name: String,
    --     upperThreshold: Float,
    --     lowerThreshold: Float,
    --     metricType: MetricType,
    --     values: List Value,
    --     averageValue: Float
    Encode.object
        [ 
            ( "name", Encode.string metric.name ),
            ( "upperTreshold", Encode.float metric.upperThreshold ),
            ( "lowerTreshold", Encode.float metric.lowerThreshold ),
            ( "averageValue", Encode.float metric.averageValue ),
            ( "values", Encode.list encodeMetricValue metric.values )
        ]

encodeMetricValue: Metric.Value -> Encode.Value
encodeMetricValue val =
    Encode.object[
        ("value", Encode.float val.value),
        ("parentDeclaration", Encode.string val.parentDeclaration)
    ]

view: Model -> Html Msg
view model =
    div[][
        div[ class "header" ][
            h1[][ text "Checkpoints"],
            div[ class "subtext" ][ text "Follow the changes in your project through checkpoints."]
        ],
        div[ class "main-header" ][
            text "Header"
        ],
        h2[][ text "Project progression "],
        div[ class "explanation" ][ text """This tool allows the user to set specific checkpoints, which can be used to track the project's metric progression over a period of time.
            This feature can be helpful when looking to refactor and restructure code to improve it's overall quality.
            """ ],
        if List.length model.checkpoints == 0 then
            div[ class "main-header" ][ text "No checkpoints created for this project so far." ]
        else
            div[class "main-cards"][
                div[ class "card" ][],
                div[ class "card "][]
            ]
        , h2[][ text "Set checkpoint" ],
        div[ class "explanation" ][ text "Set a new unique checkpoint for Elm Metrics to track." ],
        button[ class "button-special", onClick SetCheckpoint ][ text "Set Checkpoint" ],
        div[ class "card", style "margin" "25px" ][
            text (encode 0 (dictToJson model.metrics))
        ]
    ]

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model