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
import Loading exposing (LoaderType(..), defaultConfig, render)
import Elm.Parser exposing (parse)
import Elm.RawFile exposing (..)
import Elm.Syntax.File exposing (..)
import Elm.Processing exposing (process, init)
import Update.Extra exposing (andThen)
import List.Extra exposing (getAt, setAt)
import List exposing (length)

--load the files
--take only .elm or .json, otherwise from them out
--read their content, convert it into String and update the parent state with its values.
--add filter - accept a list of extensions that it should/should accept, when * accepts every extension!

type alias MyFile =
  { 
    buff : Maybe File.File,
    path: String, 
    name: String, 
    content : String,
    ast : Maybe Elm.RawFile.RawFile,
    status: Status
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
  = ChooseDirectory (List File.File)
  | ReadFiles
  | FileReadSuccess Int String String
  | ParseFile Int

type Status
  --use this to load the files
  = Idle
  | Loading
  | Success

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseDirectory files ->
            --now this is amazing, allows to chain messages...didnt know this existed
            update ReadFiles { model | files = List.filter (isValid model.extensions) (convertFiles files), status = Loading}

        ReadFiles ->
            ( { model | files = List.map(\file -> 
              {file | name = 
                case file.buff of
                  Just buff ->
                    File.name buff
                  Nothing ->
                    ""
              }) model.files },
            List.indexedMap (\index file ->
                case file.buff of
                  Just buff -> 
                    File.toString buff |> Task.perform (FileReadSuccess index (File.name buff))
                  Nothing ->
                    Debug.todo "can't happen"
              ) model.files |> Cmd.batch
            )

        FileReadSuccess index name content ->
          update (ParseFile index) 
            { model | files =
              List.map
                  (\file ->
                    case file.buff of
                      Just buff ->
                        if File.name buff == name then
                            --somehow extract the path here
                          { file | content = content }
                        else
                          file
                      Nothing ->
                        file
                  )
                  model.files
            }
        
        ParseFile index ->
        --am I braindead?
          ({ model | files = 
            case getAt index model.files of
              Just file ->
                let
                  res = parse file.content
                in
                  case res of
                    Ok rawFile ->
                      setAt index { file | ast = Just rawFile, buff = Nothing, status = Success } model.files
                    Err _ ->
                      model.files
              Nothing ->
                model.files,
              status = 
                if index == 0 then
                  Success
                else
                  Loading
          }, Cmd.none) 
          -- |> andThen update Succeed
        
view : Model -> Html Msg
view model =
  let folderInput =
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
          ][],
          button[ class "bold" ][ text "Upload project" ]     
        ]
  in
    case model.status of
      Idle ->
        folderInput
      Loading ->
        div[][
            h3[][text "Loading"],
            div[ class "subtext" ][text "Processing your files..."],
            div[ 
                style "position" "fixed",
                style "left" "0px",
                style "top" "0px",
                style "height" "100%",
                style "width" "100%",
                style "background-color" "gray",
                style "z-index" "1",
                style "opacity" "0.2"
            ][],
            div[] ( List.map(\file -> 
              case file.status of
                Idle ->
                  text ""
                Loading ->
                  div[][ 
                    text "Loading.." ,
                    Loading.render Loading.Circle {defaultConfig | size = 25} Loading.On |> Html.Styled.fromUnstyled
                  ]
                Success ->
                  div[][ text file.name ]
             ) model.files),
            Loading.render Loading.Circle {defaultConfig | size = 100} Loading.On |> Html.Styled.fromUnstyled
        ]
      Success ->
        div[][
            h3[][ text ("Loaded files (" ++ String.fromInt (List.length model.files) ++ ")") ],
            hr[ style "margin" "auto" ][],
            List.map (
              \file -> div [][ 
                p [][ 
                  text file.name 
                ]
              ]
            ) 
            model.files |> div[]
            -- h3[][ text "Upload another project" ],
            -- hr[][],
            -- div[ style "margin-bottom" "10px" ][ folderInput ]
        ]

filesDecoder =
  Decode.at [ "target", "files" ] (Decode.list File.decoder)

pathDecoder =
  Decode.at ["webkitRelativePath"] Decode.string

isValid: List String -> MyFile -> Bool
isValid extensions file =
  let
    filename = 
      case file.buff of
        Just buff ->
          File.name buff
        Nothing ->
          ""
  in
    if List.length extensions == 1 then
      if Regex.contains (Regex.fromString (String.append (String.join "" extensions) "$") |> Maybe.withDefault Regex.never) filename then
        True
      else
        False  
    else
      if Regex.contains (Regex.fromString (String.join "|" (List.map (\ext -> ext ++ "$") extensions)) |> Maybe.withDefault Regex.never) filename then
        True
      else
        False

convertFiles: List File.File -> List MyFile
convertFiles files =
  List.map(\file -> MyFile (Just file) "" "" "" Nothing Idle) files

initMyFile: File.File -> MyFile
initMyFile file =
  MyFile (Just file) "" "" "" Nothing Idle

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model

-- getFiles: Model -> List File.File
-- getFiles model =
--   List.map getFile model.files

-- getFile: MyFile -> File.File
-- getFile file =
--   file.buff

getFilesContent: Model -> List MyFile
getFilesContent model =
  model.files

