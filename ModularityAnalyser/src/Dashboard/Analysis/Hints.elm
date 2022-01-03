module Dashboard.Analysis.Hints exposing (..)

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
    section[ class "grid" ][
        h1[][ text "Hints"],
        div[ class "subtext" ][ text "Collection of hints for improving your code."],
        article[][
        ]
    ]

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model