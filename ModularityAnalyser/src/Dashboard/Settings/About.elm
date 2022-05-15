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
        div[ class "main-header" ][],
        h2[][ text "Introduction"],
        div[ class "explanation" ][
            text "This project was created as a result of solving my Master's thesis on FIIT STU 2022."
        ],
        h2[][ text "Used approaches"],
        div[ class "explanation" ][
            text """This analytical tool is one of the obtained results, along with the metrics and their threshold values.
            Threshold values were calculated over the dataset of few Elm modules by using quartiles. """
        ],
        h2[][ text "Obtained results"],
        div[ class "explanation" ][
            text """ Metrics and the analytical tool were
            used to objectively compare approaches to integrating reusable components. Results of this comparation are a key part of this work,
            along with this tool and defined metrics. """
        ]
    ]
    
getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model