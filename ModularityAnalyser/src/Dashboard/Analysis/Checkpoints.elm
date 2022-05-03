module Dashboard.Analysis.Checkpoints exposing (..)

import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Analyser.Metrics.Metric exposing (Metric)
import Dict exposing (Dict)

type alias Model = 
    {
        checkpoints: List Checkpoint
    }

type Msg
    = NoOp
    | Delete --remove checkpoints from Local Storage
type alias Checkpoint =
    {
        projectId: String,
        metrics: Dict String Metric,
        timestamp: String
    }

init: ( Model, Cmd Msg )
init =
    ({ checkpoints = [] }, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        Delete ->
            (model, Cmd.none)

view: Model -> Html Msg
view model =
    div[][
        div[ class "header" ][
            h1[][ text "Checkpoints"],
            div[ class "subtext" ][ text "Follow the changes in your project through checkpoints."]
        ],
        div[ class "main-header" ][
            text "No checkpoints located in Local Storage."
        ]
    ]

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model