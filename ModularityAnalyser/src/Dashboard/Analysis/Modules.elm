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
import Regex exposing (..)
import Analyser.File exposing (File_)
import Analyser.AST.Helper as Helper exposing (..)
import SyntaxHighlight exposing (useTheme, monokai, gitHub, elm, toBlockHtml)
import Json.Decode as Decode exposing(Error)
import Json.Encode as Encode
import JsonTree
import Set exposing (Set, toList)
import Elm.RawFile exposing (..)
import List.Extra exposing (getAt)
import Analyser.AST.Declaration exposing (viewDeclarations)
import Dict exposing (Dict, map, toList, values, keys)
import Elm.Syntax.Exposing exposing (..)



type alias Model = 
    {
        files: List File_,
        search: String,
        mode: Mode,
        astTreeState: JsonTree.State,
        parsedJson: (Result Decode.Error JsonTree.Node)
    }

type Msg
    = NoOp
    | UpdateSearch String
    | SwapMode File_
    | SetTreeState JsonTree.State
    | Back

type Mode
    = Detail File_
    | Overview

init: List File_ -> ( Model, Cmd Msg)
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
             case model.mode of
                Overview ->
                    ({ 
                        model | mode = Detail file
                        , astTreeState = 
                            case model.parsedJson of
                                Ok node ->
                                    JsonTree.defaultState
                                    -- ((JsonTree.collapseToDepth 1) node model.astTreeState)
                                _ ->
                                    JsonTree.defaultState
                        , parsedJson =  case file.ast of
                            Just ast ->
                                JsonTree.parseString """{}"""
                                -- JsonTree.parseString (Encode.encode 0 (Elm.RawFile.encode ast) )
                            _ ->
                                JsonTree.parseString """{}"""
                    }, Cmd.none)
                Detail _ ->
                    ({ model | mode = Overview, astTreeState =  JsonTree.defaultState, parsedJson = JsonTree.parseString """{}"""}, Cmd.none)
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
                div[][
                    article[ class "main-header"][
                        text "No Elm project currently loaded."
                    ]
                ]
            else
                case model.mode of
                    Overview ->
                        div[][
                            div[ class "main-header"][
                            ],
                            h2[ style "margin" "25px" ][ text "Module list" ],
                            div[ class "explanation" ][
                                input [ id "search", value model.search, placeholder "Search...", onInput UpdateSearch ] []
                            ],
                            div[ class "main-overview" ](
                                List.map (\file ->
                                    if String.contains (String.toLower model.search) (String.toLower file.name) then
                                        lazy viewModuleCard file
                                    else
                                        text ""
                                ) model.files
                            )
                        ]
                
                    Detail file ->
                        div[][
                                viewModuleDetailContent file model
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

viewModuleCard: File_ -> Html Msg
viewModuleCard file =
    case file.ast of
        Nothing ->
            text ""
        Just ast ->
            div[ onClick (SwapMode file), class "overviewcard" ][
                let
                    moduleName = Helper.getModuleNameRaw ast
                    arr = String.split "." moduleName
                in
                    if List.length arr > 2 then
                        text (String.join "." (List.drop 1 arr))
                    else
                        text moduleName
            ]

viewModuleDetailContent: File_ -> Model -> Html Msg
viewModuleDetailContent file model =
    case file.ast of
        Just ast ->
            div[][
                hr[ style "width" "100%" ][],
                div[ class "header" ][
                    h1[][ text (getModuleNameFromAst ast) ],
                    button [ onClick Back, class "button-special", style "float" "right", style "margin" "5px" ][ text "Back" ]
                ],
                div[ class "main-header"][],
                div[ class "body" ][
                    h2[][ text "Exposed declarations" ],
                    hr[ style "width" "20%" ][],
                    div[ class "div-special" ] <| (List.map (\val -> text (val ++ ", ")) (Helper.getAllDeclarations ast)),
                    h2[][ text "Used modules" ],
                    hr[ style "width" "20%" ][],
                    let
                        usedModules = keys file.calledModules
                    in
                        if List.length usedModules == 0 then
                            div[ class "explanation" ][ text "None" ]
                        else
                            div[ class "div-special" ] ( List.map(\mod -> div[][ text mod ]) usedModules )
                        ,h2[][ text "Used by modules" ],
                        hr[ style "width" "20%" ][],
                        let
                            usedByModules = Set.toList file.calledByModules
                        in
                            if List.length usedByModules == 0 then
                                div[ class "explanation" ][ text "None" ]
                            else
                                div[ class "div-special" ] ( List.map(\mod -> div[][ text mod ]) usedByModules )
                        ,h2[][ text "Declarations" ],
                        hr[ style "width" "20%" ][],
                        List.map(\dec -> viewDeclarations dec) (file.declarations) |> div[],
                        h2[][ text "Source code" ],
                        hr[ style "width" "20%" ][],
                        div[][ 
                            useTheme gitHub,
                            elm file.content
                                |> Result.map (toBlockHtml (Just 0))
                                |> Result.withDefault
                                    (pre [] [ code [] [ text file.content ]])
                        ]
                        -- h2[][ text "Abstract Syntax Tree" ],
                        -- viewJsonTree file.name model,
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
    

viewModuleDetail: File_ -> Model -> Html Msg
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