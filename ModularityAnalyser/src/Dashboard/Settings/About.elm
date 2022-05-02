module Dashboard.Settings.About exposing (..)

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
            h2[] [ text "About" ],
            div[ class "subtext" ][ text "Basic information about the project." ]
        ],
        div[ class "main-header" ][
            text "Header"
        ],
        div[ class "explanation" ][
            text "This project was created during solving my Master's thesis on FIIT STU 2022."
        ]
    ]
    
getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model