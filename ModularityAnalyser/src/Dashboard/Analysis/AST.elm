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
                    input [ id "search", type_ "text", class "form-control", Html.Attributes.value model.search, onInput Search ] [],
                    List.map( \file -> 
                            case file.ast of
                                Nothing ->
                                    text ""
                                Just ast ->
                                    if String.contains model.search file.name then
                                        article[ style "max-width" "30%" ][
                                            h3[][ text file.name],
                                            text (ASTHelper.joinPath ast),
                                            --List.map text (ASTHelper.getImports ast) |> div[],
                                            List.map (\line -> 
                                                pre[][ code[] [ text line ] ]
                                            ) (String.lines file.content) |> div[]
                                        ]
                                    else
                                        text ""
                                        --(List.map displayModulePath (moduleNameToString (moduleName ast))) |> div[],
                                        -- h2[][ text (moduleName ast)],
                                        -- button[][ text "Show source code" ],
                                        -- button[][ text "Show AST" ],
                                        -- button[][ text "Hide comments" ],
                                        --text (Debug.toString (ASTHelper.getCommentLines (ASTHelper.processRawFile ast)))
                                        --pre[][ code[][ text file.content]]
                    ) model.files |> div[]
                ]
            ]

displayModulePath: String -> Html msg
displayModulePath string =
    text(string ++ "/")

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model