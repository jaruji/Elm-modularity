module Analyser.AST.Boilerplate exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Debug exposing (toString)
import List

type alias Boilerplate_ =
    {
        range: (Int, Int),
        type_: Type_,
        moduleName: Maybe String,
        argument: String
    }

type Type_
    = Function_ String
    | Type_ String
    | Body_
    | Default

init: (Int, Int) -> Type_ -> Maybe String -> String -> Boilerplate_ 
init range type_ name arg =
    { range = range, type_ = type_, moduleName = name, argument = arg}

default: Boilerplate_
default =
    (Boilerplate_ (0,0) Default Nothing "")