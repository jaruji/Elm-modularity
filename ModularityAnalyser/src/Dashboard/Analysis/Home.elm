module Dashboard.Analysis.Home exposing (..)

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
            FileSelector.view (FileSelector.getModel model.projectFiles) |> Html.map FileSelectorMsg
            -- label[ attribute "for" "project", class "my-file-upload" ][ 
            --     text "Upload project directory"
            -- ],
            -- input [ attribute "id" "filepicker", 
            -- attribute "directory" "", 
            -- attribute "multiple" "", 
            -- attribute "accept" ".elm",
            -- type_ "file", 
            -- attribute "webkitdirectory" "" ][ 
            --     text "Choose directory"
            -- ]
        ],
        article[][
            -- ul [ attribute "id" "listing" ][]
        ],
        article[][],
        article[][]
    ]

readFiles : File -> Cmd Msg
readFiles file =
  Task.perform FileContentLoaded (File.toString file)


getFiles: Model -> List File
getFiles model =
    FileSelector.getFiles (FileSelector.getModel model.projectFiles)

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model