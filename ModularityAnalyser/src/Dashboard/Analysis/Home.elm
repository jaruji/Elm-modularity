module Dashboard.Analysis.Home exposing (..)

import Color exposing (Color)
import Material.Icons.Navigation exposing (close)
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition, linear)
import Css.Animations exposing (keyframes, property)
import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Styled exposing(toUnstyled)
import Time exposing (..)
import File exposing (..)
import Task
import Dashboard.Components.FileSelector as FileSelector exposing (..)

type alias Model = 
    {
        fileSelector: (FileSelector.Model, Cmd FileSelector.Msg),
        content: String
    }

type Msg
    = NoOp
    | FileSelectorMsg FileSelector.Msg
    | FileContentLoaded String


init: ( Model, Cmd Msg)
init =
    ({ fileSelector = FileSelector.init [".elm", ".json"], content = ""}, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FileSelectorMsg mesg ->
            fileSelectorHelper model (FileSelector.update mesg (FileSelector.getModel model.fileSelector))
        FileContentLoaded fileContent ->
            ({ model | content = fileContent}, Cmd.none)
        _ ->
            (model, Cmd.none)

fileSelectorHelper: Model -> (FileSelector.Model, Cmd FileSelector.Msg) -> (Model, Cmd Msg)
fileSelectorHelper model (fileSelector, cmd) =
  ({ model | fileSelector = (fileSelector, cmd) }, Cmd.map FileSelectorMsg cmd)

view: Model -> Html Msg
view model =
    section[ class "grid" ][
        h1[][ text "Welcome to ElMetrics"],
        div[ class "subtext" ][ text "Analytic tool to measure the quality of your Elm codebase."],
        article[][
            h2[] [ text "Getting started" ],
            text "Please, select an Elm project directory that you want to analyse by clicking the button ",
            span[ class "bold" ][ text "Upload Folder " ],
            text "button.",
            hr[][],
            -- h4[][ text "The solution for modular Elm code"],
            FileSelector.view (FileSelector.getModel model.fileSelector) |> toUnstyled |> Html.map FileSelectorMsg 
        ]
        -- article[][],
        -- article[][],
        -- article[][]
    ]

readFiles : File -> Cmd Msg
readFiles file =
  Task.perform FileContentLoaded (File.toString file)


getFiles: Model -> List FileSelector.MyFile
getFiles model =
    FileSelector.getFilesContent (FileSelector.getModel model.fileSelector)

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model