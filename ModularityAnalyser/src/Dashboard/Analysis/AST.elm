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

exampleJsonInput = """{"ahoj": 15, "trapko": {"ahoj": 15, "trapko":25}}"""


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
    | ShowCard Int
    | UpdateSearch String
    | SwapMode
    | SetTreeState JsonTree.State
    | Parse String

type Mode
    = AbstractSyntaxTree
    | Code

init: List MyFile -> ( Model, Cmd Msg)
init files =
    ({
        files = files, search = "",
        mode = Code,
        astTreeState = JsonTree.defaultState,
        parsedJson = JsonTree.parseString exampleJsonInput
    }, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        UpdateSearch val ->
            ({ model | search = val }, Cmd.none)
        ShowCard index ->
            (model, Cmd.none)
        SwapMode ->
            ({ model | mode = 
                case model.mode of
                    Code ->
                        AbstractSyntaxTree
                    AbstractSyntaxTree ->
                        Code

            }, Cmd.none)
        SetTreeState state ->
            ({ model | astTreeState = state }, Cmd.none)
        Parse json ->
            ({ model | parsedJson = JsonTree.parseString json}, Cmd.none)

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
                            input [ id "search", value model.search, placeholder "Search", onInput UpdateSearch ] [],
                            button [ onClick SwapMode, class "button-special" ][ text "Swap" ]
                        ],
                        section[ class "main-cards" ] (List.map (viewCard model) model.files)
                    ]
                ]
        ]

viewJsonTree: Model -> String -> Html Msg
viewJsonTree model json = 
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
                        text "Failed to parse AST"
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
                        AbstractSyntaxTree ->
                        --here make it so ast view is through an expandable tree
                            --viewJsonTree model
                            text (Encode.encode 1 (Elm.RawFile.encode ast))
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