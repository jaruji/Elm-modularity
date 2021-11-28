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
        projectFiles: (FileSelector.Model, Cmd FileSelector.Msg),
        content: String
    }

type Msg
    = NoOp
    | FileSelectorMsg FileSelector.Msg
    | FileContentLoaded String


init: ( Model, Cmd Msg)
init =
    ({ projectFiles = FileSelector.init [".elm", ".json"], content = ""}, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FileSelectorMsg mesg ->
            fileSelectorHelper model (FileSelector.update mesg (FileSelector.getModel model.projectFiles))
        FileContentLoaded fileContent ->
            ({ model | content = fileContent}, Cmd.none)
        _ ->
            (model, Cmd.none)

fileSelectorHelper: Model -> (FileSelector.Model, Cmd FileSelector.Msg) -> (Model, Cmd Msg)
fileSelectorHelper model (fileSelector, cmd) =
  ({ model | projectFiles = (fileSelector, cmd) }, Cmd.map FileSelectorMsg cmd)

view: Model -> Html Msg
view model =
    section[ class "grid" ][
        article[][
            h1[][ text "Welcome to ElMetrics"],
            text "Welcome to my little game",
            -- h4[][ text "The solution for modular Elm code"],
            FileSelector.view (FileSelector.getModel model.projectFiles) |> toUnstyled |> Html.map FileSelectorMsg 
        ],
        article[][],
        article[][],
        article[][]
    ]

readFiles : File -> Cmd Msg
readFiles file =
  Task.perform FileContentLoaded (File.toString file)


getFiles: Model -> List (String, String)
getFiles model =
    FileSelector.getFilesContent (FileSelector.getModel model.projectFiles)

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model