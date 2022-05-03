module Dashboard.Analysis.Modules exposing (..)

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
import Analyser.AST.Helper as Helper exposing (..)
import SyntaxHighlight exposing (useTheme, monokai, gitHub, elm, toBlockHtml)
import Json.Decode as Decode exposing(Error)
import Json.Encode as Encode
import JsonTree
import Set exposing (Set)
import Elm.RawFile exposing (..)
import List.Extra exposing (getAt)
import Analyser.AST.Parser exposing (parseRawFile, parseFile)
import Analyser.AST.Declaration exposing (viewDeclarations)
import Dict exposing (Dict, map, toList, values)


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
    | SwapMode MyFile
    | SetTreeState JsonTree.State
    | Back

type Mode
    = Detail MyFile
    | Overview

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
        mode = Overview,
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
        SwapMode file ->
            ({ model | mode = 
                case model.mode of
                    Overview ->
                        Detail file
                    Detail _ ->
                        Overview
                , astTreeState = 
                    case model.parsedJson of
                        Ok node ->
                            ((JsonTree.collapseToDepth 1) node model.astTreeState)
                        _ ->
                            JsonTree.defaultState
                , parsedJson =  case file.ast of
                    Just ast ->
                        JsonTree.parseString (Encode.encode 0 (Elm.RawFile.encode ast) )
                    _ ->
                        JsonTree.parseString """{}"""
            }, Cmd.none)
        SetTreeState state ->
            ({ model | astTreeState = state }, Cmd.none)
        Back ->
            ({ model | mode = Overview}, Cmd.none)

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
                            Detail file ->    
                                lazy2 viewModuleDetail file model
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
            div[ onClick (SwapMode file), class "overviewcard" ][
                text file.name
            ]

viewModuleDetailContent: MyFile -> Model -> Html Msg
viewModuleDetailContent file model =
    case file.ast of
        Just ast ->
            div[][
                div[ class "header" ][
                    h1[][ text (getModuleNameFromAst ast) ],
                    button [ onClick Back, class "button-special", style "float" "right", style "margin" "5px" ][ text "Back" ]
                ],
                hr[ style "width" "100%" ][],
                div[ class "body" ][
                    h2[][ text "Imports" ],
                    let
                        imports = Helper.getImportList ast
                    in
                        Dict.map(\key val -> div[][ text (key ++ " : " ++ Debug.toString val) ]) imports |> values |> div[],
                    ---------------------------------------------------------------------------------------------------------------------------------
                    ---------------------------------------------------------------------------------------------------------------------------------
                    ---------------------------------------------------------------------------------------------------------------------------------
                    ---------------------------------------------------------------------------------------------------------------------------------
                    ---------------------------------------------------------------------------------------------------------------------------------
                    h2[][ text "EXPOSING MATCHING" ],
                    div[][
                        let
                            raws = List.foldl
                                (\f acc->
                                    case f.ast of
                                        Nothing ->
                                            acc
                                        Just val ->
                                            val :: acc
                                ) [] model.files
                        in
                            List.foldl(\raw acc -> 
                                acc ++ List.map(\imp -> 
                                    let
                                        exp = getExposing imp
                                        mod = "viewModuleDetail"
                                        impName =  String.join "." (Helper.getNodeValue imp.moduleName)
                                    in
                                        case Helper.exposes mod exp of
                                            True ->
                                                div[][
                                                    text ( impName ++ " exposes " ++ mod)
                                                ]
                                            _ ->
                                                div[][
                                                    text ( impName ++ " doesn't expose " ++ mod)
                                                ]
                                )(imports raw) 
                            ) [] raws |> div[]
                        
                    ],
                    ---------------------------------------------------------------------------------------------------------------------------------
                    ---------------------------------------------------------------------------------------------------------------------------------
                    ---------------------------------------------------------------------------------------------------------------------------------
                    ---------------------------------------------------------------------------------------------------------------------------------
                    ---------------------------------------------------------------------------------------------------------------------------------
                    h2[][ text "Debug" ],
                    List.map(\dec -> viewDeclarations dec) (parseRawFile ast) |> div[],
                    h2[][ text "Source code" ],
                    div[][ 
                        useTheme gitHub,
                        elm file.content
                            |> Result.map (toBlockHtml (Just 1))
                            |> Result.withDefault
                                (pre [] [ code [] [ text file.content ]])
                    ],
                    -- h2[][ text "Abstract Syntax Tree" ],
                    -- viewJsonTree file.name model,
                    h2[][ text "Exposed declarations" ],
                    text (Debug.toString(Helper.getAllDeclarations ast))
                    -- let
                    --     functions = List.filter(\val -> Helper.filterFunction val) (Helper.getAllDeclarations ast)
                    -- in
                    --     div[](
                    --         List.map (\val -> text ((Helper.getFunctionLOC val) |> Debug.toString)) functions
                    --     )
                ]
            ]
        Nothing ->
            text "Something went wrong"
    

viewModuleDetail: MyFile -> Model -> Html Msg
viewModuleDetail file model =
    div[][
        div[
        style "position" "absolute",
        style "min-width" "100%",
        style "min-height" "100%",
        style "top" "0px",
        style "left" "0px",
        style "background-color" "grey",
        style "opacity" "0.8",
        style "z-index" "100",
        style "cursor" "pointer",
        onClick Back
        ][],
        div[ class "modal"] [
            viewModuleDetailContent file model
        ]
    ]
    

displayModulePath: String -> Html msg
displayModulePath string =
    text(string ++ "/")

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model