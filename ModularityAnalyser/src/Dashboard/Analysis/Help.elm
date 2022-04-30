module Dashboard.Analysis.Help exposing (..)

import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)

type alias Model = 
    {
        
    }

type Msg
    = NoOp


init: ( Model, Cmd Msg)
init =
    ({}, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

view: Model -> Html Msg
view model =
    div[][
        div[ class "header" ][
            h1[][ text "Help"],
            div[ class "subtext" ][ text "Basic information about used metrics, what they measure and how were the threshold values obtained."]
        ],
        div[ class "main-header" ][
            text "Metrics list..."
        ]
    ]

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model