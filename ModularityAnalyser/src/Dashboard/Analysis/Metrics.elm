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
import List.Extra exposing (find, indexedFoldl)
import Debug exposing (toString)
import File.Download as Download
import Json.Encode as Encode exposing (dict)

{--
    This module contains the visualization and overall display of metric values. Metrics are calculated in Home page right after the modules are successfully loaded into the app.
    Metric values in this modules are obtained from Main module at init.
--}

type alias Model = 
    {
        files: List File_,
        page: Page,
        metrics: Dict String Metric,
        state: State,
        heatmap: Chart.Model
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
    | ChartMsg Chart.Msg


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
        state = Idle,
        heatmap = Chart.init
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
        ChartMsg chartMsg ->
            ({ model | heatmap = Chart.update chartMsg model.heatmap }, Cmd.none)

constructCsv: Dict String Metric -> String
constructCsv metrics =
    Dict.foldl(\key _ acc -> acc ++ key ++ ",") "" metrics
    ++ 
    "\n"
    ++ List.foldl(\metric acc1-> 
        acc1 ++ (
            List.foldl(\val acc2 -> 
            acc2 ++ (val.value |> Debug.toString) ++ ","
            ) "" metric.values
        ) ++ "\n"
    ) "" (values metrics)

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
                        ],
                        div[ style "margin" "auto", style "float" "center" ][
                            button [ onClick (Swap Local), class "button-special", if model.page == Local then class "button-special-selected" else class "" ][ text "Modules"],
                            button [ onClick (Swap Global), class "button-special", if model.page == Global then class "button-special-selected" else class "" ][ text "Project averages" ]
                        ],
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
                                    h2[ style "margin" "25px" ][ text "Modularity heatmap"],
                                    -- div[] <| (List.map(\val -> div[] <| List.map(\val2 -> text ((val2 |> Debug.toString) ++ " ")) val ) (constructAdjacencyMatrix model.files)),
                                    div[ class "explanation"][
                                        text "Heatmap visualizing the number of calls between each module. Color changes based on the severity of the dependency between two modules."
                                    ],
                                    -- div[ class "card" ][
                                    --     let
                                    --         fileNames =
                                    --             List.filterMap(\val ->
                                    --                 case val.ast of
                                    --                     Just ast ->
                                    --                         Just (Helper.getModuleNameRaw ast)
                                    --                     Nothing ->
                                    --                         Nothing
                                    --             ) files
                                    --     in
                                    --         table[ style "width" "100%", style "text-align" "left"][
                                    --             thead[][
                                    --                 tr[] <| List.map(\name -> th[ class "rotate" ][ div[][ span[][ text name ]] ]) fileNames
                                    --             ]
                                    --         ]
                                    -- ],
                                    div[][
                                        div[][
                                            let
                                                fileNames =
                                                    List.filterMap(\val ->
                                                        case val.ast of
                                                            Just ast ->
                                                                Just (Helper.getModuleNameRaw ast)
                                                            Nothing ->
                                                                Nothing
                                                    ) files
                                            in
                                                (Chart.viewHeatmap model.heatmap (constructAdjacencyMatrix model.files) fileNames) |> Html.map ChartMsg
                                        ]
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
                                                div[ class "explanation" ][ text metric.description ],
                                                h3[][ text "Histogram" ],
                                                div[ class "chart-cards" ][
                                                    div[ class "chartcard" ][
                                                        Chart.viewMetricBarplot metric.name metric.averageValue metric.lowerThreshold metric.upperThreshold metric.values
                                                    ]
                                                ]
                                            ]
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
                                    ],
                                    div[ class "card" ][
                                        projectTableContent2 model.metrics
                                    ]
                                ]
                            
                    ]
                    
    ]

constructAdjacencyMatrix: List File_ -> List (List Int)
constructAdjacencyMatrix files =
    let
        fileNames = 
            List.filterMap(\val ->
                case val.ast of
                    Just ast ->
                        Just (Helper.getModuleNameRaw ast)
                    Nothing ->
                        Nothing
            ) files
        calledModules =
            List.filterMap(\val ->
                case val.ast of
                    Just ast ->
                        Just val.calledModules
                    Nothing ->
                        Nothing
            ) files
    in    
        indexedFoldl(\index dict acc ->
            (List.foldl(\name acc2 ->
                case Dict.get name dict of
                    Just value ->
                        value :: acc2
                    Nothing ->
                        0 :: acc2
            ) [] fileNames ) :: acc
        ) [] calledModules

    -- List.foldl(\mod1 index1 acc1 -> 

    -- ) [] calledModules

    -- indexedFoldl(\mod1 index1 acc1 -> 
    --     indexedFoldl(\mod2 index2 acc2 -> 
    --         if Dict.get mod1 
    --     ) (keys calledModules)
    -- ) [] (keys calledModules)

localTableContent: Dict String Metric -> String -> Html msg
localTableContent metrics name  =
    List.foldl(\metric acc ->
        case find(\val -> if(val.parentDeclaration == name) then True else False) (Metric.getValues metric) of
            Just num ->
                acc ++ [ td[ if metric.upperThreshold < num.value || metric.lowerThreshold > num.value then style "color" "red" else class "" ][ text (num.value |> toString) ]] 
            Nothing ->
                acc ++ [ td [][ text "--" ] ]
    ) [td[] [ text name ]] (values metrics) |> tr[]
    
projectTableContent: Dict String Metric -> Html msg
projectTableContent metrics =
    table[ style "text-align" "left", style "width" "100%"][
        tr[][
            th[][ text "Metric" ],
            th[][ text "Value" ]
        ],
        (List.map(\val -> tr[][ td[][ text ("Average " ++ val.name)], td[][ text (val.averageValue |> toString) ]]) (values metrics)) |> tbody[]
    ]

projectTableContent2: Dict String Metric -> Html msg
projectTableContent2 metrics =
    table[ style "text-align" "left", style "width" "100%"][
        tr[][
            th[][ text "Metric" ],
            th[][ text "Value" ]
        ],
        (List.map(\val -> tr[][ td[][ text ("Median of " ++ val.name)], td[][ text (val.medianValue |> toString) ]]) (values metrics)) |> tbody[]
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