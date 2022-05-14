module Dashboard.Analysis.Boilerplate exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Analyser.File exposing (File_)
import Analyser.AST.Helper as Helper exposing (getModuleNameRaw)
import Analyser.AST.Parser as Parser exposing (findBoilerplateRaw)
import Analyser.AST.Boilerplate exposing (Boilerplate_)

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
                        ],
                        div[](List.map(\val -> 
                            case val.ast of
                                Just ast ->
                                    div[][
                                        h2[][ text (getModuleNameRaw ast)],
                                        div[] <| List.map(\node -> viewBoilerplate node) (Parser.findBoilerplateRaw ast model.files)
                                    ]
                                _ ->
                                    text ""
                        ) model.files )
                            
                    ]
                    
    ]

viewBoilerplate: Boilerplate_ -> Html msg
viewBoilerplate boilerplate =
    div[][
        text (boilerplate.name),
        text "  ",
        text (String.join "." boilerplate.argument),
        text (" - " ++ Debug.toString boilerplate.range)
    ]

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model