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
import Dashboard.Components.FileSelector as FileSelector exposing (MyFile)

import Ports exposing (request)
import Dashboard.Analysis.Home as Home exposing (..)
import Dashboard.Analysis.AST as AST exposing (Model, update, view)
import Dashboard.Analysis.Dependencies as Dependencies exposing (Model, update, view)
import Dashboard.Analysis.Metrics as Metrics exposing (Model, update, view)
import Dashboard.Analysis.Modules as Modules exposing (Model, update, view)
import Dashboard.Analysis.Help as Help exposing (Model, update, view)
import Dashboard.Settings.Settings as Settings exposing (Model, update, view)
import Dashboard.Settings.Preferences as Preferences exposing (Model, update, view)
import Dashboard.Settings.About as About exposing (Model, update, view)

---- MODEL ----

type alias Model =
    {
        dashboard: Dashboard,
        files: List MyFile
    }

type Dashboard
    = HomePage Home.Model
    | ASTPage AST.Model
    | DependenciesPage Dependencies.Model
    | MetricsPage Metrics.Model
    | ModulePage Modules.Model
    | HelpPage Help.Model
    | SettingsPage Settings.Model
    | PreferencesPage Preferences.Model
    | AboutPage About.Model
    


init : ( Model, Cmd Msg )
init =
    ( { dashboard = HomePage (Home.getModel (Home.init Nothing)), files = []}, Cmd.none )


---- UPDATE ----

type Msg
    = NoOp
    | ChangePage Dashboard
    | HomeMsg Home.Msg
    | ASTMsg AST.Msg
    | DependenciesMsg Dependencies.Msg
    | MetricsMsg Metrics.Msg
    | ModuleMsg Modules.Msg
    | HelpMsg Help.Msg
    | SettingsMsg Settings.Msg
    | PreferencesMsg Preferences.Msg
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
        ASTMsg mesg ->
            case model.dashboard of
                ASTPage ast ->
                    astHelper model (AST.update mesg ast)
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
        ModuleMsg mesg ->
            case model.dashboard of
                ModulePage mod ->
                     ({ model | dashboard = ModulePage mod }, Cmd.none)
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
        PreferencesMsg mesg ->
            case model.dashboard of
                PreferencesPage pref ->
                     ({ model | dashboard = PreferencesPage pref }, Cmd.none)
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
  ({ model | dashboard = HomePage home, files = Home.getFiles home }, Cmd.map HomeMsg cmd)

astHelper: Model -> (AST.Model, Cmd AST.Msg) -> (Model, Cmd Msg)
astHelper model (ast, cmd) =
  ({ model | dashboard = ASTPage ast }, Cmd.map ASTMsg cmd)

metricsHelper: Model -> (Metrics.Model, Cmd Metrics.Msg) -> (Model, Cmd Msg)
metricsHelper model (metrics, cmd) =
  ({ model | dashboard = MetricsPage metrics }, Cmd.map MetricsMsg cmd)


---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div []
        [ 
            viewNav model,
            section[ class "page-content" ][
                case model.dashboard of
                    HomePage home ->
                        Home.view home |> Html.map HomeMsg
                    ASTPage ast ->
                        AST.view ast |> Html.map ASTMsg
                    DependenciesPage dep ->
                        Dependencies.view dep |> Html.map DependenciesMsg
                    MetricsPage met ->
                        Metrics.view met |> Html.map MetricsMsg
                    ModulePage mod ->
                        Modules.view mod |> Html.map ModuleMsg
                    HelpPage hint ->
                        Help.view hint |> Html.map HelpMsg
                    SettingsPage set ->
                        Settings.view set |> Html.map SettingsMsg
                    PreferencesPage pref ->
                        Preferences.view pref |> Html.map PreferencesMsg
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
        a[ href "#home", onClick (ChangePage (HomePage (Home.getModel (Home.init (getLoadedFiles model))))) ][ img [ src "/logo.svg", class "logo" ] [] ],
        nav[][
            ul [ class "menu" ][

                li[ class "menu-heading"][
                    h3[][
                        text "Analysis"
                    ]
                ],

                li[][ button[ onClick (ChangePage (HomePage (Home.getModel (Home.init (getLoadedFiles model))))),
                    case model.dashboard of 
                        HomePage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.home 20 Inherit ], text "Home" ]],

                li[][ button[ onClick (ChangePage (ASTPage (AST.getModel (AST.init model.files)))),
                    case model.dashboard of 
                        ASTPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.account_tree 20 Inherit ], text "AST" ]],
                
                li[][ button[ onClick (ChangePage (DependenciesPage (Dependencies.getModel (Dependencies.init (getElmJson model.files))))),
                    case model.dashboard of 
                        DependenciesPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.inventory_2 20 Inherit ], text "Dependencies" ]],

                li[][ button[ onClick (ChangePage (MetricsPage (Metrics.getModel (Metrics.init model.files)))),
                    case model.dashboard of 
                        MetricsPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Filled.analytics 20 Inherit ], text "Metrics" ]],

                li[][ button[ onClick (ChangePage (ModulePage (Modules.getModel (Modules.init model.files)))),
                    case model.dashboard of 
                        ModulePage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.view_module 20 Inherit ], text "Module Diagram" ]],

                li[][ button[ onClick (ChangePage (HelpPage (Help.getModel Help.init))),
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

                li[][ button[ onClick (ChangePage (PreferencesPage (Preferences.getModel Preferences.init))),
                    case model.dashboard of 
                        PreferencesPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.psychology 20 Inherit ], text "Preferences" ]],

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

isElmJson: MyFile -> Bool
isElmJson file =
    if file.name == "elm.json" then
        True
    else
        False

getElmJson: List MyFile -> Maybe String
getElmJson files =
    let
        result = List.Extra.find isElmJson files
    in
        case result of
            Nothing ->
                Nothing
            Just val ->
                Just val.content

getLoadedFiles: Model -> Maybe (List MyFile)
getLoadedFiles model =
    if length model.files == 0 then
        Nothing
    else
        Just model.files
    
---- PROGRAM ----

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
