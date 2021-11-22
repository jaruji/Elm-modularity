module Main exposing (..)

import Browser
import Browser.Navigation as Nav exposing (pushUrl)
import Color exposing (Color)
import Material.Icons exposing(home, close)
import Color
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (..)
import Dashboard.Home as Home exposing (..)

---- MODEL ----


type alias Model =
    {
        dashboard: Dashboard
    }

type Dashboard
    = HomePage Home.Model
    | DependenciesPage
    | MetricsPage
    | ModulePage
    | HintsPage
    | SettingsPage
    | PreferencesPage
    


init : ( Model, Cmd Msg )
init =
    ( { dashboard = HomePage (Home.getModel Home.init) }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | ChangePage Dashboard
    | HomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeMsg mesg ->
            case model.dashboard of
                HomePage home ->
                    homeHelper model (Home.update mesg home)
                _ ->
                    (model, Cmd.none)
        ChangePage page ->
            ({ model | dashboard = page}, Cmd.none)
        _ ->
            ( model, Cmd.none )

homeHelper: Model -> (Home.Model, Cmd Home.Msg) -> (Model, Cmd Msg)
homeHelper model (home, cmd) =
  ({ model | dashboard = HomePage home }, Cmd.map HomeMsg cmd)


---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div []
        [ 
            Html.h1 [] [ Html.text "Elmetrics" ],
            viewNav model,
            Home.view (Home.getModel Home.init) |> Html.map HomeMsg
        ]


-- (Color <| Color.rgb255 255 255 255)
viewNav: Model -> Html Msg
viewNav model =
    header [ class "navHeader"]
    [
        img [ src "/logo.svg", class "logo" ] [],
        nav[][
            ul [ class "menu" ][
                li[ class "menu-heading"][
                    h3[][
                        text "Admin"
                    ]
                ],

                li[][ button[ onClick (ChangePage (HomePage (Home.getModel Home.init))),
                    case model.dashboard of 
                        HomePage x -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.home 20 Inherit ], text "Home" ]],
                li[][ button[ onClick (ChangePage MetricsPage),
                
                    case model.dashboard of 
                        MetricsPage -> class "selected"
                        _ -> class ""
                    ][ span[][ Filled.analytics 20 Inherit ], text "Software Metrics" ]],

                li[][ button[ onClick (ChangePage DependenciesPage),
                    case model.dashboard of 
                        DependenciesPage -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.account_tree 20 Inherit ], text "Dependencies" ]],

                li[][ button[ onClick (ChangePage ModulePage),
                    case model.dashboard of 
                        ModulePage -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.view_module 20 Inherit ], text "Module Diagram" ]],

                li[][ button[ onClick (ChangePage HintsPage),
                    case model.dashboard of 
                        HintsPage -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.lightbulb 20 Inherit ], text "Hints" ]],

                li[ class "menu-heading"][
                    h3[][
                        text "Settings"
                    ]
                ],

                li[][ button[ onClick (ChangePage SettingsPage),
                    case model.dashboard of 
                        SettingsPage -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.settings 20 Inherit ], text "Settings" ]],

                li[][ button[ onClick (ChangePage PreferencesPage),
                    case model.dashboard of 
                        PreferencesPage -> class "selected"
                        _ -> class ""
                    ][ span[][ Outlined.psychology 20 Inherit ], text "Preferences" ]]
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
