module Dashboard.Analysis.Boilerplate exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Analyser.File exposing (File_)
import Analyser.AST.Helper as Helper exposing (getModuleNameRaw)
import Analyser.AST.Parser as Parser exposing (findBoilerplateRaw)
import Analyser.AST.Boilerplate exposing (Boilerplate_, Type_(..))

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
                        h2[][ text "Modules" ],
                        div[ class "explanation" ][
                            text "There are several criteria for detecting boilerplate types in Elm modules:",
                            div[][
                                text "1. The module has to have a type that uses a type of a external module."
                            ],
                            div[][
                                text "2. A type alias of this external module has to be present in the type alias of our module"
                            ],
                            text "The boilerplate detection alghoritm detects only one boilerplate pattern. It is not foolproof, so it is possible that the users will get some false positive / false negative results."
                        ],
                        div[](List.map(\val -> 
                            case val.ast of
                                Just ast ->
                                    let
                                        boilerplate = Parser.findBoilerplateRaw ast model.files
                                    in 
                                        case List.length boilerplate of
                                            0 ->
                                                text ""
                                            _ ->
                                                div[ class "card", style "width" "100%", style "min-height" "auto" ][
                                                    h2[][ text (getModuleNameRaw ast)],
                                                    div[] <| List.map(\node -> viewBoilerplate node) boilerplate
                                                    --pre [] [ code [] [ text val.content ]]
                                                ]
                                _ ->
                                    text ""
                        ) model.files )
                            
                    ]
                    
    ]

viewBoilerplate: Boilerplate_ -> Html msg
viewBoilerplate boilerplate =
    div[][
        case boilerplate.type_ of
            Type_ a ->
                text (a)
            _ ->
                text ""
        , text "  "
        , case boilerplate.moduleName of
            Nothing ->
                text ""
            Just moduleName ->
                text (String.join "." [moduleName, boilerplate.argument])
        , text (" - " ++ Debug.toString boilerplate.range)
    ]

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model