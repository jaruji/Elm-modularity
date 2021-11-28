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

parse : String -> String
parse input =
  case Elm.Parser.parse input of
    Err err ->
      "Error: " ++ Debug.toString err
    Ok resp ->
      Debug.toString resp

type alias Model = 
    {
        files: List (String, String)
    }

type Msg
    = NoOp


init: List (String, String) -> ( Model, Cmd Msg)
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
                text "No project loaded"
            ]
        ]
        else
            section[ class "grid" ][
                List.map (
                \file -> div [][
                    if Regex.contains (Regex.fromString (".elm$") |> Maybe.withDefault Regex.never) (Tuple.first file) then
                        article[][
                            h3[][ text (Tuple.first file)],
                            text (parseTuple file),
                            hr[][]
                        ]
                    else
                        text ""
                ]
                ) model.files |> div[]
            ]

parseTuple: (String, String) -> String
parseTuple tuple =
    parse (Tuple.second tuple)

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model