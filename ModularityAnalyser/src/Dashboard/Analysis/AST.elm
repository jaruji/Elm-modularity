module Dashboard.Analysis.AST exposing (..)

import Color exposing (Color)
import Elm.Parser
import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import File exposing (..)
import Task
import Tuple exposing (..)
import List exposing (..)
import Regex exposing (..)
import Elm.Syntax.ModuleName
import Elm.RawFile exposing (moduleName, imports)
import Dashboard.Components.FileSelector as FileSelector exposing (MyFile)


type alias Model = 
    {
        files: List MyFile
    }

type Msg
    = NoOp


init: List MyFile -> ( Model, Cmd Msg)
init files =
    ({files = files}, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

view: Model -> Html Msg
view model =
    let
        len = List.length model.files
    in
        if len == 0 then
        section[ class "grid" ][
            article[][
                text "No Elm project currently loaded."
            ]
        ]
        else
            section[ class "grid" ][
                h1[][ text "Abstract syntax tree"],
                div[ class "subtext" ][ text "Preview of loaded modules and their AST's."],
                List.map ( \file -> 
                    div [][
                        case file.ast of
                            Nothing ->
                                text ""
                            Just ast ->
                                div[][
                                    h3[][ text file.name]
                                ]
                        -- let
                        --     ast = parseTuple file
                        -- in
                        --     if Regex.contains (Regex.fromString (".elm$") |> Maybe.withDefault Regex.never) (Tuple.first file) then
                        --         article[][
                        --             h3[][ text (Tuple.first file)],
                        --             -- text (moduleName ast),
                        --             text ast,
                        --             hr[][]
                        --         ]
                        --     else
                        --         text ""
                    ]
                ) model.files |> article[]
            ]

-- parseTuple: (String, String) -> String
-- parseTuple tuple =
--     parse (Tuple.second tuple)

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model