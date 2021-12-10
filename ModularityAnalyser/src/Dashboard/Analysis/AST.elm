module Dashboard.Analysis.AST exposing (..)

import Color exposing (Color)
import Elm.Parser
import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import File exposing (..)
import Task
import Tuple exposing (..)
import List exposing (..)
import List.Extra exposing (..)
import Regex exposing (..)
import Dashboard.Components.FileSelector as FileSelector exposing (MyFile)
import Analyser.ASTHelper as ASTHelper exposing (..)


type alias Model = 
    {
        files: List MyFile,
        search: String
    }

type Msg
    = NoOp
    | ShowCard Int
    | Search String


init: List MyFile -> ( Model, Cmd Msg)
init files =
    ({files = files, search = ""}, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        Search string ->
            ({ model | search = string }, Cmd.none)
        ShowCard index ->
            (model, Cmd.none)

view: Model -> Html Msg
view model =
    let
        len = List.length model.files
    in
        if len == 0 then
        section[ class "grid" ][
            h1[][ text "Abstract syntax tree"],
            div[ class "subtext" ][ text "Preview of loaded modules and their AST's."],
            article[][
                text "No Elm project currently loaded."
            ]
        ]
        else
            section[ class "grid" ][
                div[][
                    h1[][ text "Abstract syntax tree"],
                    div[ class "subtext" ][ text "Preview of loaded modules and their AST's."],
                    label [ for "search" ] [ text "Search:" ],
                    input [ id "search", type_ "text", Html.Attributes.value model.search, onInput Search ] [],
                    section[ class "codeSnippet" ] (List.map viewCard model.files)
                ]
            ]

viewCard: MyFile -> Html Msg
viewCard file =
    case file.ast of
        Nothing ->
            text ""
        Just ast ->
            article[ style "max-width" "100%" ][
                h2[][ text file.name],
                hr[][],
                text (ASTHelper.joinPath ast),
                text (Debug.toString (ASTHelper.getDeclarationLines (ASTHelper.processRawFile ast))),
                --List.map text (ASTHelper.getImports ast) |> div[],
                List.map (\line -> 
                    pre[][ code[] [ text line ] ]
                ) (String.lines file.content) |> div[]
            ]


displayModulePath: String -> Html msg
displayModulePath string =
    text(string ++ "/")

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model