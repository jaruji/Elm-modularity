module Dashboard.Analysis.Checkpoints exposing (..)

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

lightMode: Attribute Msg
lightMode =
    style "background-color" "white"

darkMode: Attribute Msg
darkMode =
    style "background-color" "dark"