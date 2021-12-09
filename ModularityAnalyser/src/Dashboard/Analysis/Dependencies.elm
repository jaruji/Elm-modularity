module Dashboard.Analysis.Dependencies exposing (..)

import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Analyser.Dependency as Dependency exposing (..)
import Json.Decode as Decode exposing (decodeString)

type alias Model = 
    {
        json: Maybe String,
        dependencies: Maybe Dependency.Model
    }

type Msg
    = NoOp
    | Decode Dependency.Model


init: Maybe String -> ( Model, Cmd Msg)
init json =
    ({json = json, dependencies = Nothing }, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        Decode dependencies ->
            ({ model | dependencies = Just dependencies }, Cmd.none)

view: Model -> Html Msg
view model =
    section[ class "grid" ][
        h1[][ text "Project dependencies"],
        div[ class "subtext" ][ text "All the dependencies of your Elm project in one place."],
        article[][
            case model.json of
                Nothing ->
                    text "Elm.json not loaded"
                Just val ->
                    case decodeString Dependency.decodeElmJson val of
                        Ok result ->
                            Dependency.view result
                        Err err ->
                            text ("Error while loading elm.json file")
                            --Debug.toString err
        ]
    ]

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model