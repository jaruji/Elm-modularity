module Dashboard.Analysis.AST exposing (..)

import Color exposing (Color)
import Elm.Parser
import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
                h1[][ text "Abstract syntax tree"],
                div[ class "subtext" ][ text "Preview of loaded modules and their AST's."]
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
                            input [ id "search", value model.search, placeholder "Search", onInput UpdateSearch ] []
                        ],
                        div[ class "main-cards"][
                            case model.mode of
                                AbstractSyntaxTree name ->    
                                    div [ class "card", style "width" "100%" ][
                                        viewJsonTree name model 
                                    ]
                                Code ->
                                    div[](List.map (viewCard model) model.files)
                        ]
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
                        h2[ style "margin" "25px" ][ text name],
                        button [ onClick Back, class "button-special", style "float" "right", style "margin" "5px" ][ text "Back" ],
                        hr[ style "width" "100%" ][],
                        JsonTree.view node (config) model.astTreeState
                    ]
                                
                Err _ ->
                    div[][
                        text "Failed to this AST"
                    ]
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
                    -- text (ASTHelper.joinPath ast),
                    -- text (Debug.toString (ASTHelper.getDeclarationLines (ASTHelper.processRawFile ast))),
                    --List.map text (ASTHelper.getImports ast) |> div[],
                    case model.mode of
                        Code ->
                        --visualize a code snippet
                            List.map (\line -> 
                                pre[][ code[] [ text line ] ]
                            ) (String.lines file.content) |> article[ style "padding" "10px" ]
                        _ ->
                            text ""
                        --here make it so ast view is through an expandable tree
                        --viewJsonTree model
                        -- text (Encode.encode 1 (Elm.RawFile.encode ast))
                        -- List.map (\line -> 
                        --     pre[][ code[] [ text line ] ]
                        -- ) (String.lines (Debug.toString ast)) |> div[]
                ]
            else
                text ""

displayModulePath: String -> Html msg
displayModulePath string =
    text(string ++ "/")

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model