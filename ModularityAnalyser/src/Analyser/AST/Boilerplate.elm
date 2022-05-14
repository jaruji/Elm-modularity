module Analyser.AST.Boilerplate exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Debug exposing (toString)
import List

type alias Boilerplate_ =
    {
        range: (Int, Int),
        type_: Type_,
        name: String,
        argument: List String
    }

type Type_
    = Function_
    | Type_
    | Body_
    | Default

init: (Int, Int) -> Type_ -> String -> List String -> Boilerplate_ 
init range type_ name arg =
    { range = range, type_ = type_, name = name, argument = arg}

default: Boilerplate_
default =
    (Boilerplate_ (0,0) Default "" [])