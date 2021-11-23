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
--testComment
parse : String -> String
parse input =
  case Elm.Parser.parse input of
    Err err ->
      "Error: " ++ Debug.toString err
    Ok resp ->
      Debug.toString resp
"""

parse : String -> String
parse input =
  case Elm.Parser.parse input of
    Err err ->
      "Error: " ++ Debug.toString err
    Ok resp ->
      Debug.toString resp

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