module Main exposing (..)

import Browser
import Browser.Navigation as Nav exposing (pushUrl)
import Color exposing (Color)
import Color
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import File exposing (..)
import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (..)
import List.Extra exposing(iterate, find)
import List exposing (length)
import Dict exposing (Dict, fromList)
import Analyser.Metrics.Metric exposing (Metric)
import Analyser.File exposing (File_)
import Analyser.AST.Helper exposing (getAllDeclarations)
import Dashboard.Analysis.Home as Home exposing (..)
import Dashboard.Analysis.Modules as Modules exposing (Model, update, view)
import Dashboard.Analysis.Dependencies as Dependencies exposing (Model, update, view)
import Dashboard.Analysis.Metrics as Metrics exposing (Model, update, view)
import Dashboard.Analysis.ModuleDiagram as ModuleDiagram exposing (Model, update, view)
import Dashboard.Analysis.Help as Help exposing (Model, update, view)
import Dashboard.Settings.Settings as Settings exposing (Model, update, view)
import Dashboard.Analysis.Checkpoints as Checkpoints exposing (Model, update, view)
import Dashboard.Settings.About as About exposing (Model, update, view)

---- MODEL ----

type alias Model =
    {
        dashboard: Dashboard,
        files: List File_,
        metrics: Dict String Metric
    }

type Dashboard
    = HomePage Home.Model
    | ModulesPage Modules.Model
    | DependenciesPage Dependencies.Model
    | MetricsPage Metrics.Model
    | ModuleDiagramPage ModuleDiagram.Model
    | HelpPage Help.Model
    | SettingsPage Settings.Model
    | CheckpointsPage Checkpoints.Model
    | AboutPage About.Model
    


init : ( Model, Cmd Msg )
init =
    ( { dashboard = HomePage (Home.getModel (Home.init [])), files = [], metrics = fromList []}, Cmd.none )


---- UPDATE ----

type Msg
    = NoOp
    | ChangePage Dashboard
    | HomeMsg Home.Msg
    | ModulesMsg Modules.Msg
    | DependenciesMsg Dependencies.Msg
    | MetricsMsg Metrics.Msg
    | ModuleDiagramMsg ModuleDiagram.Msg
    | HelpMsg Help.Msg
    | SettingsMsg Settings.Msg
    | CheckpointsMsg Checkpoints.Msg
    | AboutMsg About.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeMsg mesg ->
            case model.dashboard of
                HomePage home ->
                    homeHelper model (Home.update mesg home)
                _ ->
                    (model, Cmd.none)
        ModulesMsg mesg ->
            case model.dashboard of
                ModulesPage mod ->
                    modulesHelper model (Modules.update mesg mod)
                _ ->
                    (model, Cmd.none)
        DependenciesMsg mesg ->
            case model.dashboard of
                DependenciesPage dep ->
                     ({ model | dashboard = DependenciesPage dep }, Cmd.none)
                _ ->
                    (model, Cmd.none)
        MetricsMsg mesg ->
            case model.dashboard of
                MetricsPage met ->
                    metricsHelper model (Metrics.update mesg met)
                _ ->
                    (model, Cmd.none)
        ModuleDiagramMsg mesg ->
            case model.dashboard of
                ModuleDiagramPage mod ->
                    moduleDiagramHelper model (ModuleDiagram.update mesg mod)
                _ ->
                    (model, Cmd.none)
        HelpMsg mesg ->
            case model.dashboard of
                HelpPage hint ->
                     ({ model | dashboard = HelpPage hint }, Cmd.none)
                _ ->
                    (model, Cmd.none)
        SettingsMsg mesg ->
            case model.dashboard of
                SettingsPage set ->
                     ({ model | dashboard = SettingsPage set }, Cmd.none)
                _ ->
                    (model, Cmd.none)
        CheckpointsMsg mesg ->
            case model.dashboard of
                CheckpointsPage pref ->
                     ({ model | dashboard = CheckpointsPage pref }, Cmd.none)
                _ ->
                    (model, Cmd.none)
        AboutMsg mesg ->
            case model.dashboard of
                AboutPage abo ->
                     ({ model | dashboard = AboutPage abo }, Cmd.none)
                _ ->
                    (model, Cmd.none)
        ChangePage page ->
            ({ model | dashboard = page}, Cmd.none)
        _ ->
            ( model, Cmd.none )

homeHelper: Model -> (Home.Model, Cmd Home.Msg) -> (Model, Cmd Msg)
homeHelper model (home, cmd) =
  ({ model | dashboard = HomePage home, files = Home.getFiles home, metrics = Home.getMetrics home }, Cmd.map HomeMsg cmd)

modulesHelper: Model -> (Modules.Model, Cmd Modules.Msg) -> (Model, Cmd Msg)
modulesHelper model (mod, cmd) =
  ({ model | dashboard = ModulesPage mod }, Cmd.map ModulesMsg cmd)

moduleDiagramHelper: Model -> (ModuleDiagram.Model, Cmd ModuleDiagram.Msg) -> (Model, Cmd Msg)
moduleDiagramHelper model (mod, cmd) =
  ({ model | dashboard = ModuleDiagramPage mod }, Cmd.map ModuleDiagramMsg cmd)

metricsHelper: Model -> (Metrics.Model, Cmd Metrics.Msg) -> (Model, Cmd Msg)
metricsHelper model (metrics, cmd) =
  ({ model | dashboard = MetricsPage metrics }, Cmd.map MetricsMsg cmd)


---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div []
        [ 
            viewNav model,
            section[ class "dashboard" ][
                case model.dashboard of
                    HomePage home ->
                        Home.view home |> Html.map HomeMsg
                    ModulesPage mod ->
                        Modules.view mod |> Html.map ModulesMsg
                    DependenciesPage dep ->
                        Dependencies.view dep |> Html.map DependenciesMsg
                    MetricsPage met ->
                        Metrics.view met |> Html.map MetricsMsg
                    ModuleDiagramPage mod ->
                        ModuleDiagram.view mod |> Html.map ModuleDiagramMsg
                    HelpPage hint ->
                        Help.view hint |> Html.map HelpMsg
                    SettingsPage set ->
                        Settings.view set |> Html.map SettingsMsg
                    CheckpointsPage pref ->
                        Checkpoints.view pref |> Html.map CheckpointsMsg
                    AboutPage abo ->
                        About.view abo |> Html.map AboutMsg
                    -- _ ->
                    --     text "Hello world"
            ]
        ]


-- (Color <| Color.rgb255 255 255 255)
viewNav: Model -> Html Msg
viewNav model =
    header [ class "navHeader"]
    [
        a[ href "#home", onClick (ChangePage (HomePage (Home.getModel (Home.init model.files)))) ][ img [ src "/logo.svg", class "logo" ] [] ],
        nav[][
            ul [ class "menu" ][

                li[ class "menu-heading"][
                    h3[][
                        text "Analysis"
                    ]
                ],

                li[][ button[ onClick (ChangePage (HomePage (Home.getModel (Home.init model.files)))),
                    case model.dashboard of 
                        HomePage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.home 20 Inherit ], text "Home" ]],

                li[][ button[ onClick (ChangePage (ModulesPage (Modules.getModel (Modules.init model.files)))),
                    case model.dashboard of 
                        ModulesPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.account_tree 20 Inherit ], text "Modules" ]],
                
                li[][ button[ onClick (ChangePage (DependenciesPage (Dependencies.getModel (Dependencies.init (getElmJson model.files))))),
                    case model.dashboard of 
                        DependenciesPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.inventory_2 20 Inherit ], text "Dependencies" ]],

                li[][ button[ onClick (ChangePage (MetricsPage (Metrics.getModel (Metrics.init model.files model.metrics)))),
                    case model.dashboard of 
                        MetricsPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Filled.analytics 20 Inherit ], text "Metrics" ]],

                li[][ button[ onClick (ChangePage (ModuleDiagramPage (ModuleDiagram.getModel (ModuleDiagram.init model.files)))),
                    case model.dashboard of 
                        ModuleDiagramPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.view_module 20 Inherit ], text "Module Diagram" ]],

                li[][ button[ onClick (ChangePage (CheckpointsPage (Checkpoints.getModel (Checkpoints.init model.metrics)))),
                    case model.dashboard of 
                        CheckpointsPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.psychology 20 Inherit ], text "Checkpoints" ]],

                li[][ button[ onClick (ChangePage (HelpPage (Help.getModel (Help.init model.metrics)))),
                    case model.dashboard of 
                        HelpPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.lightbulb 20 Inherit ], text "Help" ]],

                li[ class "menu-heading"][
                    h3[][
                        text "Settings"
                    ]
                ],

                li[][ button[ onClick (ChangePage (SettingsPage (Settings.getModel Settings.init))),
                    case model.dashboard of 
                        SettingsPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.settings 20 Inherit ], text "Settings" ]],

                li[][ button[ onClick (ChangePage (AboutPage (About.getModel About.init))),
                    case model.dashboard of 
                        AboutPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.info 20 Inherit ], text "About" ]],

                div[ class "credits" ][
                    li[ class "menu-heading"][
                        h3[][
                            text "Credits"
                        ]
                    ],
                    li[][
                        button[][  span[][ Outlined.copyright 20 Inherit ], text "Juraj Bedej" ]
                    ],
                    li[][
                        button[][a[ href "https://github.com/jaruji/Elm-modularity/tree/main", attribute "target" "_blank"][ 
                            span[][ Outlined.source 20 Inherit ], text "GitHub" 
                            ]
                        ]
                    ]
                ]

            ]
        ]
    ]

isElmJson: File_ -> Bool
isElmJson file =
    if file.name == "elm.json" then
        True
    else
        False

getElmJson: List File_ -> Maybe String
getElmJson files =
    let
        result = List.Extra.find isElmJson files
    in
        case result of
            Nothing ->
                Nothing
            Just val ->
                Just val.content
    
---- PROGRAM ----

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
