module Dashboard.Analysis.Boilerplate exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Analyser.File exposing (File_)
import Analyser.AST.Helper
import Analyser.AST.Parser

type alias Model = 
    {
        files: List File_
    }

type Msg
    = NoOp


init: List File_ -> ( Model, Cmd Msg)
init files =
    ({ files = files }, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

view: Model -> Html Msg
view model =
    div[][
        div[ class "header" ][
            h1[][ text "Boilerplate"],
            div[ class "subtext" ][ text "Detection of boilerplate code in your modules."]
        ],
            case List.length model.files of
                0 ->
                    div[ class "main-header" ][
                        text "No Elm modules loaded."
                    ]
                _ ->
                    div[][
                        div[ class "main-header" ][],
                        div[ class "explanation" ][
                            text "The purpose of this page is to provide functionality regarding boilerplate pattern detection in Elm modules."
                        ]
                    ]
                    
    ]

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model