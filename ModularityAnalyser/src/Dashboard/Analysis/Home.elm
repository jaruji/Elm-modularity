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
import List exposing (length)
import List.Extra exposing (find)
import Html.Events exposing (onClick)
import Task
import Dashboard.Components.FileSelector as FileSelector exposing (..)
import Analyser.Metrics.Metric as Metric exposing (..)
import Dict exposing (Dict, map, values, keys, fromList)

type alias Model = 
    {
        fileSelector: (FileSelector.Model, Cmd FileSelector.Msg),
        files: List MyFile,
        status: Status
    }

type Msg
    = NoOp
    | FileSelectorMsg FileSelector.Msg
    | Remove

type Status 
    = Idle
    | Loading
    | Success (Dict String Metric)

init: List MyFile -> ( Model, Cmd Msg)
init files =
    ({ fileSelector = FileSelector.init [".elm", ".json"], files = files, status = Idle}, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FileSelectorMsg mesg ->
            fileSelectorHelper model (FileSelector.update mesg (FileSelector.getModel model.fileSelector))
        Remove ->
            ({ model | files = []}, Cmd.none)
        _ ->
            (model, Cmd.none)

fileSelectorHelper: Model -> (FileSelector.Model, Cmd FileSelector.Msg) -> (Model, Cmd Msg)
fileSelectorHelper model (fileSelector, cmd) =
    let 
        newModel =
            case fileSelector.status of
                FileSelector.Loading ->
                    { model | status = Loading } 
                FileSelector.Success ->
                    { model | status = Success (calculateMetrics fileSelector.files), files = fileSelector.files}
                _ ->
                    { model | status = Idle }
    in
        ({ newModel | fileSelector = (fileSelector, cmd) }, Cmd.map FileSelectorMsg cmd)


view: Model -> Html Msg
view model =
    div[][
        div[ class "header" ][
            h1[][ text "Welcome to Elm Metrics"],
            div[ class "subtext" ][ text "Analytical tool to measure the quality of your Elm codebase through the usage of software metrics and numerous visualizations."]
        ],
        div[ class "main-header" ][
            text "Header"
        ],
        
        div[][
            case length model.files of
                0 ->
                    div[][
                        div[ class "main-cards" ][
                            div[ class "card", style "width" "100%" ][
                                div[][
                                    h2[] [ text "Getting started" ],
                                    text "Please, select an Elm project directory that you want to analyse by clicking the ",
                                    span[ class "bold" ][ text "Upload Folder " ],
                                    text "button.",
                                    -- h4[][ text "The solution for modular Elm code"],
                                    div[ style "text-align" "center", style "margin" "15px" ][
                                        FileSelector.view (FileSelector.getModel model.fileSelector) |> toUnstyled |> Html.map FileSelectorMsg
                                    ]
                                ]
                            ]
                        ]        
                    ]
                _ ->
                    div[ class "main-cards"][
                        div[ class "card", style "text-align" "center" ][
                            h2[][ text "A project is currently loaded in the tool." ],
                            hr[ class "customhr" ][],
                            div[](List.map(\file -> div[][ text file.name ]) model.files),
                            hr[ class "customhr" ][],
                            button[onClick Remove, class "button-special", style "background-color" "darkred", style "margin" "15px" ][ 
                                text "Remove project" 
                            ]
                        ],
                        div[ class "card" ][
                            h2[][ text "Basic information" ],
                            hr[ class "customhr" ][],
                            div[][
                                b[][ text "Loaded module count: " ],
                                let
                                    filtered = 
                                        List.filter(\file -> 
                                            case file.ast of
                                                Nothing ->
                                                    False
                                                Just _ ->
                                                    True
                                        ) model.files
                                in
                                    text (List.length filtered |> Debug.toString)
                            ],
                            div[][
                                b[][ text "Project type: " ],
                                let
                                    filtered = find(\val -> if val.name == "elm.json" then True else False) model.files
                                in
                                    case filtered of
                                        Just val ->
                                            text (getProjectType val.content)
                                        Nothing ->    
                                            text "Elm.json not loaded"
                                
                            ],
                            div[][
                                b[][ text "File tree: " ]
                            ]
                           
                        ]
                    ]
                    
        ]
    ]

getProjectType: String -> String
getProjectType json =
    if String.contains "\"type\": \"application\"" json then
        "Application"
    else
        "Module"
    

getFiles: Model -> List FileSelector.MyFile
getFiles model =
    FileSelector.getFilesContent (FileSelector.getModel model.fileSelector)

getMetrics: Model -> Dict String Metric
getMetrics model= 
    case model.status of
        Success metrics ->
            metrics
        _ ->
            fromList []

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model