module Dashboard.Analysis.AST exposing (..)

import Color exposing (Color)
import Elm.Parser
import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy, lazy2)
import Time exposing (..)
import File exposing (..)
import Task
import Tuple exposing (..)
import List exposing (..)
import List.Extra exposing (..)
import Regex exposing (..)
import Dashboard.Components.FileSelector as FileSelector exposing (MyFile)
import Analyser.ASTHelper as ASTHelper exposing (..)
import SyntaxHighlight exposing (useTheme, monokai, elm, toBlockHtml)
import Json.Decode as Decode
import Json.Encode as Encode
import JsonTree
import Set exposing (Set)
import Elm.RawFile exposing (..)
import List.Extra exposing (getAt)

type alias Model = 
    {
        files: List MyFile,
        search: String,
        mode: Mode,
        astTreeState: JsonTree.State,
        parsedJson: (Result Decode.Error JsonTree.Node)
    }

type Msg
    = NoOp
    | UpdateSearch String
    | SwapMode String RawFile
    | SetTreeState JsonTree.State
    | Back

type Mode
    = AbstractSyntaxTree String
    | Code

init: List MyFile -> ( Model, Cmd Msg)
init files =
    ({
        files = List.filter (
            \file -> case file.ast of
                Just _ ->
                    True
                Nothing ->
                    False
        ) files, 
        search = "",
        mode = Code,
        astTreeState = JsonTree.defaultState,
        parsedJson = JsonTree.parseString """{}"""
    }, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        UpdateSearch val ->
            ({ model | search = val }, Cmd.none)
        SwapMode name ast ->
            ({ model | mode = 
                case model.mode of
                    Code ->
                        AbstractSyntaxTree name
                    AbstractSyntaxTree _ ->
                        Code
                , astTreeState = 
                    case model.parsedJson of
                        Ok node ->
                            ((JsonTree.collapseToDepth 1) node model.astTreeState)
                        _ ->
                            JsonTree.defaultState
                , parsedJson =  JsonTree.parseString (Encode.encode 1 (Elm.RawFile.encode ast) )
            }, Cmd.none)
        SetTreeState state ->
            ({ model | astTreeState = state }, Cmd.none)
        Back ->
            ({ model | mode = Code}, Cmd.none)

view: Model -> Html Msg
view model =
    let
        len = List.length model.files
    in
        div[][
            div[ class "header" ][
                h1[][ text "Modules"],
                div[ class "subtext" ][ text "All the basic information about loaded Elm modules."]
            ],
            if len == 0 then
                section[][
                    article[ class "main-header"][
                        text "No Elm project currently loaded."
                    ]
                ]
            else
                section[][
                    div[][
                        div[ class "main-header"][
                            text "Header"
                        ],
                        h2[ style "margin" "25px" ][ text "Module list" ],
                        div[ class "explanation" ][
                            input [ id "search", value model.search, placeholder "Search...", onInput UpdateSearch ] []
                        ],
                        div[ class "main-overview" ](
                            List.map (\file ->
                                if String.contains (String.toLower model.search) (String.toLower file.name) then
                                    viewModuleCard file
                                else
                                    text ""
                            ) model.files
                        ),
                        --div[](List.map (viewCard model) model.files),
                        case model.mode of
                            AbstractSyntaxTree name ->    
                                lazy2 viewModuleDetail name model
                            _ ->
                                text ""
                    ]
                ]
        ]

viewJsonTree: String -> Model -> Html Msg
viewJsonTree name model = 
    let
        config = { onSelect = Nothing, toMsg = SetTreeState, colors = JsonTree.defaultColors }
    in
        div[][
            case model.parsedJson of
                Ok node ->
                    div[ style "align-items" "start" ][
                        div[ class "header" ][
                            h1[ style "margin" "25px" ][ text name],
                            button [ onClick Back, class "button-special", style "float" "right", style "margin" "5px" ][ text "Back" ]
                        ],
                        hr[ style "width" "100%" ][],
                        JsonTree.view node (config) model.astTreeState
                    ]
                                
                Err _ ->
                    div[][
                        text "Failed to display this AST"
                    ]
        ]

viewModuleCard: MyFile -> Html Msg
viewModuleCard file =
    case file.ast of
        Nothing ->
            text ""
        Just ast ->
            div[ onClick (SwapMode file.name ast), class "overviewcard" ][
                text file.name
            ]

viewCard: Model -> MyFile -> Html Msg
viewCard model file =
    case file.ast of
        Nothing ->
            text ""
        Just ast ->
            if String.contains (String.toLower model.search) (String.toLower file.name) then
                div[ class "card" ][
                    h2[ style "padding" "25px" ][ text file.name ],
                    button [ onClick (SwapMode file.name ast), class "button-special", style "float" "right", style "margin" "5px" ][ text "Show AST" ],
                    hr[ style "width" "100%" ][],
                    List.map (\line -> 
                        pre[][ code[] [ text line ] ]
                    ) (String.lines file.content) |> article[ style "padding" "10px" ]
                ]
            else
                text ""

viewModuleDetail: String -> Model -> Html Msg
viewModuleDetail name model =
    div[][
        div[
        style "position" "absolute",
        style "min-width" "100%",
        style "min-height" "100%",
        style "top" "0px",
        style "left" "0px",
        style "background-color" "grey",
        style "opacity" "0.8",
        style "z-index" "100"
        ][],
        div[ class "modal"] [
            viewJsonTree name model
        ]
    ]
    

displayModulePath: String -> Html msg
displayModulePath string =
    text(string ++ "/")

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model