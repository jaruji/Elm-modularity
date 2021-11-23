module Main exposing (..)

import Browser
import Browser.Navigation as Nav exposing (pushUrl)
import Color exposing (Color)
import Material.Icons exposing(home, close)
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

import Dashboard.Analysis.Home as Home exposing (..)
import Dashboard.Analysis.AST as AST exposing (Model, update, view)
import Dashboard.Analysis.Dependencies as Dependencies exposing (Model, update, view)
import Dashboard.Analysis.Metrics as Metrics exposing (Model, update, view)
import Dashboard.Analysis.Modules as Modules exposing (Model, update, view)
import Dashboard.Analysis.Hints as Hints exposing (Model, update, view)
import Dashboard.Settings.Settings as Settings exposing (Model, update, view)
import Dashboard.Settings.Preferences as Preferences exposing (Model, update, view)
import Dashboard.Settings.About as About exposing (Model, update, view)

---- MODEL ----


type alias Model =
    {
        dashboard: Dashboard,
        projects: List File
    }

type Dashboard
    = HomePage Home.Model
    | ASTPage AST.Model
    | DependenciesPage Dependencies.Model
    | MetricsPage Metrics.Model
    | ModulePage Modules.Model
    | HintsPage Hints.Model
    | SettingsPage Settings.Model
    | PreferencesPage Preferences.Model
    | AboutPage About.Model
    


init : ( Model, Cmd Msg )
init =
    ( { dashboard = HomePage (Home.getModel Home.init), projects = []}, Cmd.none )


---- UPDATE ----

type Msg
    = NoOp
    | ChangePage Dashboard
    | HomeMsg Home.Msg
    | ASTMsg AST.Msg
    | DependenciesMsg Dependencies.Msg
    | MetricsMsg Metrics.Msg
    | ModuleMsg Modules.Msg
    | HintsMsg Hints.Msg
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
                     ({ model | dashboard = ASTPage ast }, Cmd.none)
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
                     ({ model | dashboard = MetricsPage met }, Cmd.none)
                _ ->
                    (model, Cmd.none)
        ModuleMsg mesg ->
            case model.dashboard of
                ModulePage mod ->
                     ({ model | dashboard = ModulePage mod }, Cmd.none)
                _ ->
                    (model, Cmd.none)
        HintsMsg mesg ->
            case model.dashboard of
                HintsPage hint ->
                     ({ model | dashboard = HintsPage hint }, Cmd.none)
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
  ({ model | dashboard = HomePage home, projects = Home.getFiles home }, Cmd.map HomeMsg cmd)


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
                    HintsPage hint ->
                        Hints.view hint |> Html.map HintsMsg
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
        a[ href "#home", onClick (ChangePage (HomePage (Home.getModel Home.init))) ][ img [ src "/logo.svg", class "logo" ] [] ],
        nav[][
            ul [ class "menu" ][

                li[ class "menu-heading"][
                    h3[][
                        text "Analysis"
                    ]
                ],

                li[][ button[ onClick (ChangePage (HomePage (Home.getModel Home.init))),
                    case model.dashboard of 
                        HomePage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.home 20 Inherit ], text "Home" ]],

                li[][ button[ onClick (ChangePage (ASTPage (AST.getModel AST.init))),
                    case model.dashboard of 
                        ASTPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.account_tree 20 Inherit ], text "AST" ]],

                li[][ button[ onClick (ChangePage (MetricsPage (Metrics.getModel Metrics.init))),
                    case model.dashboard of 
                        MetricsPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Filled.analytics 20 Inherit ], text "Software Metrics" ]],

                li[][ button[ onClick (ChangePage (DependenciesPage (Dependencies.getModel Dependencies.init))),
                    case model.dashboard of 
                        DependenciesPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.inventory_2 20 Inherit ], text "Dependencies" ]],

                li[][ button[ onClick (ChangePage (ModulePage (Modules.getModel Modules.init))),
                    case model.dashboard of 
                        ModulePage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.view_module 20 Inherit ], text "Module Diagram" ]],

                li[][ button[ onClick (ChangePage (HintsPage (Hints.getModel Hints.init))),
                    case model.dashboard of 
                        HintsPage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.lightbulb 20 Inherit ], text "Hints" ]],

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
                        button[][a[ href "https://github.com/jaruji/Elm-modularity/tree/main"][ span[][ Outlined.source 20 Inherit ], text "GitHub" ]]
                    ]
                ]

            ]
        ]
    ]


viewPageContentHeader: Html Msg
viewPageContentHeader =
    section [ class "page-content-header" ][
    ]


-- isSelected: String -> Dashboard -> Attribute Msg
-- isSelected dashboard:


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
