module Dashboard.Analysis.Metrics exposing (..)

import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Analyser.File exposing (File_)
import Analyser.AST.Helper as Helper exposing (..)
import Analyser.AST.Parser exposing (parseDeclaration)
-- import RadarChart exposing (..)
import Html.Events exposing (onClick)
import Analyser.Chart as Chart exposing (..)
import Analyser.Metrics.Metric as Metric exposing (..)
import Dict exposing (Dict, toList, fromList, map, values, keys, get)
import Svg.Attributes exposing (in_)
import List exposing (length)
import List.Extra exposing (find)
import Debug exposing (toString)
import File.Download as Download
import Json.Encode as Encode exposing (dict)


{--
    metrics:
        local:
            LOC
            Comments
            Number of types
            Number of functions
            Number of Boilerplate Msgs
            Locate MVU triplets (can only be done by name?)
            Number of imports used together with number of calls for each import

            Number of lambdas (NoL)
            -- Lambda score (LS)
            -- CPM (coupling)
            -- MAD
            -- MTD
            -- MFD
            -- Boilerplate
        global:
            ALOC
            ANOT
            ANOF
            ANOD
            ACPM
            AMAD
            AMTD
            AMFD
            Average boilerplate Msg
            Average boilerplate loc
        funkcie (ak zostane cas):
            IDEG
            OUTDEG
            ATNR
            CGDP
            CGWD
--}

type alias Model = 
    {
        files: List File_,
        page: Page,
        metrics: Dict String Metric,
        state: State
    }

type Page 
    = Global
    | Local

type State
    = Idle
    | Selected Metric

type Msg
    = NoOp
    | Swap Page
    | Export
    | Select Metric


init: List File_ -> Dict String Metric -> ( Model, Cmd Msg)
init files metrics =
    ({
        files = List.filter (
            \file -> case file.ast of
                Just _ ->
                    True
                Nothing ->
                    False
        ) files,
        page = Local,
        metrics = metrics,
        state = Idle
    }, Cmd.none)

exportCsv : String -> Cmd msg
exportCsv csv =
  Download.string "module-metrics.csv" "text/csv" csv

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        Swap page ->
            ({ model | page = page }, Cmd.none)
        Export ->
            ( model, exportCsv (constructCsv model.metrics))
        Select metric ->
            ({ model | state = Selected metric }, Cmd.none)

constructCsv: Dict String Metric -> String
constructCsv metrics =
    Dict.foldl(\key _ acc -> acc ++ key ++ ",") "" metrics
    ++ 
    "\n"

view: Model -> Html Msg
view model =
    div[][
        div[ class "header" ][
            h1[][ text "Elm metrics"],
            div[ class "subtext" ][ text "Software metrics used to compute and visualize the complexity of the project, relationships between modules and their modularity."]
        ],
        let
            files = model.files
            names = 
                List.filterMap (\file -> 
                    case file.ast of
                        Just ast ->
                            Just (Helper.getModuleNameRaw ast)
                        Nothing ->
                            Nothing
                ) files
        in
            case List.length files of
                0 ->
                    article[ class "main-header"][
                        text "No Elm project currently loaded."
                    ]
                _ ->
                    div[][
                        div[ class "main-header" ][
                            text "Header"
                        ],
                        button [ onClick (Swap Local), class "button-special", if model.page == Local then class "button-special-selected" else class "" ][ text "Modules"],
                        button [ onClick (Swap Global), class "button-special", if model.page == Global then class "button-special-selected" else class "" ][ text "Project" ],
                        case model.page of
                            Local ->
                                div[][
                                    div[][
                                        h2[ style "margin" "25px" ][ text "Module metrics"],
                                        div[ class "explanation"][
                                            text "Collection of selected metrics to quantify aspects of your Elm ",
                                            span[ class "bold" ][ text " modules." ]
                                        ],
                                        div[ class "card" ][
                                             table[ style "text-align" "left", style "width" "100%"][
                                                -- map that sets metrics table headers
                                                List.map (\val -> th[][ text val ]) ("Modules" :: (keys model.metrics)) |> tr[],
                                                ---------
                                                --map that sets metrics table content
                                                List.map (localTableContent model.metrics) names |> tbody[]
                                                ---------
                                            ]
                                        ]
                                    ],
                                    div[ style "text-align" "center" ][
                                        button[ class "button-special", onClick Export ][ text "Export CSV" ]
                                    ],
                                    h2[ style "margin" "25px" ][ text "Metrics visualization"],
                                    div[ class "explanation"][
                                        text "Collection of basic information and visualizations of selected software metrics. The red line visualizes the project average for the metric."
                                    ],
                                    div[ class "main-overview"] (
                                        List.map(\val -> 
                                            div[ class "overviewcard", onClick (Select val),
                                                case model.state of
                                                    Selected metric ->
                                                        if metric == val then
                                                            class "overviewcardHovered"
                                                        else
                                                            class ""
                                                    Idle ->
                                                        class ""
                                            ][ text val.name]
                                        ) (values model.metrics)
                                    ),
                                    case model.state of
                                        Idle ->
                                            div[ class "main-header" ][ text "No metric selected" ]
                                        Selected metric ->
                                            div[][
                                                h2[][ text metric.name],
                                                hr[][],
                                                div[ class "explanation" ][ text "Here comes the description of the metric" ],
                                                h3[][ text "Histogram" ],
                                                div[ class "chart-cards" ][
                                                    div[ class "chartcard" ][
                                                        Chart.viewMetricBarplot metric.name metric.averageValue metric.values
                                                    ]
                                                ]
                                            ]
                                    -- List.map(\metric -> 
                                    --     if length metric.values == 0 then 
                                    --         text ""
                                    --     else
                                    --         div[ class "main-overview" ][
                                    --         Chart.viewMetricBarplot metric.name metric.averageValue metric.values
                                    --     ]
                                    -- ) (values model.metrics) |> div[ class "chart-cards" ]
                                ]
                            Global ->
                                div[][
                                    h2[ style "margin" "25px" ][ text "Project metrics"],
                                    div[ class "explanation"][
                                        text "Collection of project metrics, consisting of values of metrics averaged from all ",
                                        span[ class "bold" ][ text "project " ],
                                        text "modules"
                                    ],
                                    div[ class "card" ][
                                        projectTableContent model.metrics
                                    ]
                                ]
                            
                    ]
                    
    ]

localTableContent: Dict String Metric -> String -> Html msg
localTableContent metrics name  =
    List.foldl(\metric acc ->
        case find(\val -> if(val.parentDeclaration == name) then True else False) (Metric.getValues metric) of
            Just num ->
                acc ++ [ td[][ text (num.value |> toString) ]] 
            Nothing ->
                acc ++ [ td [][ text "--" ] ]
    ) [td[] [ text name ]] (values metrics) |> tr[]
    
projectTableContent: Dict String Metric -> Html msg
projectTableContent metrics =
    table[ style "text-align" "center", style "width" "100%"][
        tr[][
            th[][ text "Metric" ],
            th[][ text "Value" ]
        ],
        (List.map(\val -> tr[][ td[][ text ("Average " ++ val.name)], td[][ text (val.averageValue |> toString) ]]) (values metrics)) |> tbody[]
    ]

removeEmpty: String -> Bool
removeEmpty string =
    if string == "" then
        False
    else
        True

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model