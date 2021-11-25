module Dashboard.Components.FileSelector exposing (..)

import Browser
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline exposing (required, optional, hardcoded)
import Regex
import String exposing (String)
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
      extensions : List String 
    }

init : List String -> ( Model, Cmd msg )
init  extensions =
    ( { files = [], extensions = extensions }, Cmd.none )


type Msg
    = ChooseDirectory (List File)
    | ReadFiles
    | FileReadSuccess String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseDirectory files ->
            ({ model | files = List.filter (isValid model.extensions) (convertFiles files)}, Cmd.none)

        ReadFiles ->
            ( model
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

view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "file"
            , attribute "webkitdirectory" ""
            , attribute "directory" ""
            , attribute "id" "filepicker"
            , on "change" (Decode.map ChooseDirectory filesDecoder)
            ]
            []
        , button [ onClick ReadFiles ] [ text "Read" ]
        , div [] <|
            List.map
                (\file ->
                    div []
                        [ p [] [ text (File.name file.buff) ]
                        -- , div[] (List.map text model.paths)
                        , text file.path
                        , text file.content
                        , hr [] []
                        ]
                )
                model.files
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

-- pathDecoder =
--   Decode.at ["target", "files", "webkitRelativePath"] (Decode.list Decode.string)

isValid: List String -> MyFile -> Bool
isValid extensions file =
  if Regex.contains (Regex.fromString (String.join "$|" extensions) |> Maybe.withDefault Regex.never) (File.name file.buff) then
    True
  else
    False
  -- if String.contains ".elm" (File.name file.buff) then
  --   True
  -- else
  --   False

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

getFilesContent: Model -> List String
getFilesContent model =
  List.map getFileContent model.files

getFileContent: MyFile -> String
getFileContent file =
  file.content

