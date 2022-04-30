module Dashboard.Analysis.Metrics exposing (..)

import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Dashboard.Components.FileSelector exposing (MyFile)
import Analyser.ASTHelper as ASTHelper exposing (..)
-- import RadarChart exposing (..)
import Html.Events exposing (onClick)
import Analyser.Chart as Chart exposing (..)
import Analyser.Metric as Metric exposing (..)
import Dict exposing (Dict, toList, fromList, map, values, keys)

{--
    metrics:
        local:
            LOC
            Comments
            Number of types
            Number of functions
            Number of Boilerplate Msgs
            Locate MVU triplets
            Number of imports used together with number of calls for each import

            Number of lambdas (NoL)
            Lambda score    (LS)
            CPM (coupling)
            MAD
            MTD
            MFD
            Boilerplate
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
        files: List MyFile,
        page: Page,
        metrics: Dict String Metric
    }

type Page 
    = Global
    | Local

type Msg
    = NoOp
    | Swap Page


init: List MyFile -> ( Model, Cmd Msg)
init files =
    ({files = files, page = Local, 
    metrics =
        fromList[
            ("LOC", Metric.init "LOC" 0 0 ModuleMetric),
            ("Comments", Metric.init "Comments" 0 0 ModuleMetric),
            ("NoD", Metric.init "NoD" 0 0 ModuleMetric),
            ("NoF", Metric.init "NoF" 0 0 ModuleMetric),
            ("NoT", Metric.init "NoT" 0 0 ModuleMetric),
            ("NoA", Metric.init "NoA" 0 0 ModuleMetric)
        ]
    }, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        Swap page ->
            ({ model | page = page }, Cmd.none)

view: Model -> Html Msg
view model =
    div[][
        div[ class "header" ][
            h1[][ text "Elm metrics"],
            div[ class "subtext" ][ text "Software metrics used to compute and visualize the complexity of the project, relationships between modules and their modularity."]
        ],
        let
            files = model.files
        in
            case List.length files of
                0 ->
                    article[ class "main-header"][
                        text "No Elm project currently loaded."
                    ]
                _ ->
                    div[][
                        div[ class "main-header" ][
                            button [ onClick (Swap Local), class "button-special", if model.page == Local then class "button-special-selected" else class "" ][ text "Modules"],
                            button [ onClick (Swap Global), class "button-special", if model.page == Global then class "button-special-selected" else class "" ][ text "Project" ]
                        ],
                        case model.page of
                            Local ->
                                div[][
                                    div[][
                                        h2[ style "margin" "25px" ][ text "Module metrics"],
                                        div[ class "card" ][
                                             table[ style "text-align" "left", style "width" "100%"][
                                                tr[][
                                                    th[][ text "Module" ],
                                                    List.map (\val -> th[][ text val ]) (keys model.metrics) |> th[]
                                                    -- th[][ text "Module"],
                                                    -- th[][ text "LOC" ],
                                                    -- th[][ text "Comments" ],
                                                    -- th[][ text "NoD" ],
                                                    -- th[][ text "NoF" ],
                                                    -- th[][ text "NoT" ],
                                                    -- th[][ text "NoA" ]
                                                ],
                                            List.map localTableContent files |> tbody[]
                                            ]
                                        ]
                                    ],
                                    h2[ style "margin" "25px" ][ text "Metrics visualization"],
                                    div[ class "main-card" ][
                                        div[ class "card" ][
                                            div[ style "height" "300px", style "width" "300px" ][
                                                Chart.viewTemplateGraph
                                            ]
                                        ],
                                        div[ class "card" ][
                                            div[ style "height" "300px", style "width" "300px" ][
                                                Chart.viewTemplateGraph
                                            ]
                                        ]
                                    ]
                                ]
                            Global ->
                                section[ style "height" "500px", style "width" "500px", style "margin-left" "50px"][
                                    text "Global mode",
                                    div[ class "card" ][
                                        Chart.viewTemplateGraph
                                    ]
                                ]
                    ]
                    
    ]

localTableContent: MyFile -> Html msg
localTableContent file =
    case file.ast of
        Just ast ->
            let 
                parsedString = String.lines file.content
                processedFile = ASTHelper.processRawFile ast
            in
                tr[][
                    td[][ text file.name ],
                    td[][ 
                        text (String.fromInt (List.length parsedString))
                    ],
                    -- td[][ 
                    --     text (String.fromInt (List.length (List.filter removeEmpty parsedString)))
                    -- ],
                    td[][
                        text (String.fromInt (List.foldl numberOfCommentedLines 0 (ASTHelper.getCommentLines processedFile)))
                    ],
                    td[][
                        text (String.fromInt (ASTHelper.numberOfDeclarations processedFile))
                    ],
                    td[][
                        text ( String.fromInt (ASTHelper.numberOfFunctions processedFile))
                    ],
                    td[][
                        text (String.fromInt (ASTHelper.numberOfTypes processedFile))
                    ],
                    td[][
                        text (String.fromInt (ASTHelper.numberOfTypeAliases processedFile))
                    ]
                ]
        Nothing ->
            text ""
        
globalTableContent: MyFile -> Html msg
globalTableContent file =
    div[][
        
    ]

removeEmpty: String -> Bool
removeEmpty string =
    if string == "" then
        False
    else
        True

numberOfCommentedLines: (Int, Int) -> Int -> Int
numberOfCommentedLines (start, end) acc =
    acc + end - start + 1

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model