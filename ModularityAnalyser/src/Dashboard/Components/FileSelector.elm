module Dashboard.Components.FileSelector exposing (..)

import Browser
import File exposing (File)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline exposing (required, optional, hardcoded)
import Regex
import String exposing (String)
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition, linear)
import Css.Animations exposing (keyframes, property)
import Svg.Styled exposing (fromUnstyled)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import Task

--load the files
--take only .elm or .json, otherwise from them out
--read their content, convert it into String and update the parent state with its values.
--add filter - accept a list of extensions that it should/should accept, when * accepts every extension!

type alias MyFile =
  { 
    buff : File,
    path: String, 
    name: String, 
    content : String 
  }

type alias Model =
  { 
    files : List MyFile,
    extensions : List String,
    status: Status
  }

init : List String -> ( Model, Cmd Msg )
init  extensions =
    ( { files = [], extensions = extensions, status = Idle }, Cmd.none )


type Msg
  = ChooseDirectory (List File)
  | ReadFiles
  | FileReadSuccess String String

type Status
  --use this to load the files
  = Idle
  | Loading
  | Success
  | Failed String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseDirectory files ->
            --now this is amazing, allows to chain messages...didnt know this existed
            update ReadFiles { model | files = List.filter (isValid model.extensions) (convertFiles files), status = Loading}

        ReadFiles ->
            ( { model | files = List.map(\file -> {file | name = File.name file.buff}) model.files, status = Success }
            , List.map
                (\file ->
                    File.toString file.buff |> Task.perform (FileReadSuccess (File.name file.buff))
                )
                model.files
                |> Cmd.batch
            )

        FileReadSuccess name content ->
            ( { model
                | files =
                    List.map
                        (\file ->
                            if File.name file.buff == name then
                                { file | content = content }
                            else
                                file
                        )
                        model.files
              }
            , Cmd.none
            )
        
        -- RemoveFiles ->
        --   ({ model | files = List.map (\file -> )})

view : Model -> Html Msg
view model =
  div [][ 
    label[ 
      css [
        cursor pointer,
        border3 (px 1) solid (hex "#888"),
        padding2 (px 6) (px 12),
        color (rgb 255 255 255),
        textAlign center,
        textDecoration none,
        fontSize (px 16),
        backgroundColor (hex "#008CBA"),
        margin auto,
        display inlineBlock,
        position relative
      ]
    ][
      input [
        css[ display none ],
        type_ "file",
        attribute "webkitdirectory" "",
        attribute "directory" "",
        attribute "id" "filepicker",
        on "change" (Decode.map ChooseDirectory filesDecoder)
      ][]
      , span[][ text "Upload folder" ]
    ],
    button [ onClick ReadFiles ] [ text "Read" ],
    List.map (
      \file -> div [][ 
        p [][ 
          text file.name ],
        --div[] (List.map text model.paths)
          text file.path,
          text file.content,
          hr [] []
      ]
    ) model.files |> div[]
  ]


-- fileDecoder: Decode.Decoder MyFile
-- fileDecoder =
--   Decode.map3 MyFile
--     (Decode.at [ "target", "files" ] File.decoder)
--     (Decode.at ["target", "files", "webkitRelativePath"] Decode.string)
--     (Decode.succeed "lmao") --default value during decoding!

--dude this is harder then the da vinci code???

-- fileDecoder: Decode.Decoder MyFile
-- fileDecoder =
--   Decode.succeed MyFile
--   |> Pipeline.required "File" File.decoder
--   |> Pipeline.required "webkitRelativePath" Decode.string
--   |> Pipeline.required "name" Decode.string
--   |> Pipeline.hardcoded ""


filesDecoder =
  Decode.at [ "target", "files" ] (Decode.list File.decoder)

-- filesDecoder =
--   Decode.at [ "target", "files" ] (Decode.list fileDecoder)

pathDecoder =
  Decode.at ["webkitRelativePath"] Decode.string

isValid: List String -> MyFile -> Bool
isValid extensions file =
  let
    filename = File.name file.buff
  in
    if List.length extensions == 1 then
      if Regex.contains (Regex.fromString (String.append (String.join "" extensions) "$") |> Maybe.withDefault Regex.never) filename then
        True
      else
        False  
    else
      if Regex.contains (Regex.fromString (String.join "$|" extensions) |> Maybe.withDefault Regex.never) filename then
        True
      else
        False

convertFiles: List File -> List MyFile
convertFiles files =
  List.map(\file -> MyFile file "" "" "") files

initMyFile: File -> MyFile
initMyFile file =
  MyFile file "" "" ""

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model

getFiles: Model -> List File
getFiles model =
  List.map getFile model.files

getFile: MyFile -> File
getFile file =
  file.buff

getFilesContent: Model -> List (String, String)
getFilesContent model =
  List.map getFileContent model.files

getFileContent: MyFile -> (String, String)
getFileContent file =
  (file.name, file.content)

