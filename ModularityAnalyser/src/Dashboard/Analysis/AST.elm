module Dashboard.Analysis.AST exposing (..)

import Color exposing (Color)
import Elm.Parser
import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import File exposing (..)
import Task

src = """module Foo exposing(foo)

foo = 1
"""

parse : String -> String
parse input =
  case Elm.Parser.parse input of
    Err e ->
      "Failed: " ++ Debug.toString e
    Ok v ->
      "Success: " ++ Debug.toString v

type alias Model = 
    {
        
    }

type Msg
    = NoOp


init: ( Model, Cmd Msg)
init =
    ({}, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

view: Model -> Html Msg
view model =
    section[ class "grid" ][
        article[][
            text (parse src)
        ]
    ]

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model