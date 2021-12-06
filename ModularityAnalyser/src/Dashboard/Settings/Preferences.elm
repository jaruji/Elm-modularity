module Dashboard.Settings.Preferences exposing (..)

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
        article[][
            h2[][ text "Preferences" ]
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