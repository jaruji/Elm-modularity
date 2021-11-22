module Dashboard.Home exposing (..)

import Color exposing (Color)
import Material.Icons.Navigation exposing (close)
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition, linear)
import Css.Animations exposing (keyframes, property)
import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import File exposing (..)
import Task
import Dashboard.Components.FileSelector as FileSelector exposing (..)

--need to remake this for elm-css (same for FileSelector component)

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
    ({ projectFiles = FileSelector.init, content = ""}, Cmd.none)

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
            FileSelector.view (FileSelector.getModel model.projectFiles) |> Html.map FileSelectorMsg
        ]
    ]

readFiles : File -> Cmd Msg
readFiles file =
  Task.perform FileContentLoaded (File.toString file)

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model