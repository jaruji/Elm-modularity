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
import Svg.Attributes exposing (in_)
import List.Extra exposing (find)
import Debug exposing (toString)


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
    ({
        files = List.filter (
            \file -> case file.ast of
                Just _ ->
                    True
                Nothing ->
                    False
        ) files,
        page = Local, 
        metrics =
            fromList[
                ("LOC", Metric.initWithValues "LOC" 0 0 ModuleMetric (calculateLOC files)),
                ("Comments", Metric.initWithValues "Comments" 0 0 ModuleMetric (calculateComments files)),
                ("NoD", Metric.initWithValues "NoD" 0 0 ModuleMetric (calculateNoD files)),
                ("NoF", Metric.initWithValues "NoF" 0 0 ModuleMetric (calculateNoF files)),
                ("NoT", Metric.initWithValues "NoT" 0 0 ModuleMetric (calculateNoT files)),
                ("NoA", Metric.initWithValues "NoA" 0 0 ModuleMetric (calculateNoA files)),
                ("Kneegan", Metric.init "Kneegan" 0 0 ModuleMetric),
                ("Allah", Metric.init "allah" 0 0 ModuleMetric)
            ]
    }, Cmd.none)

calculateLOC: List MyFile -> List Metric.Value
calculateLOC files =
    List.foldl(\file acc -> 
        case file.ast of
            Just _ ->
                let
                    parsedString = String.lines file.content
                    loc = (List.length parsedString) |> toFloat
                in
                     initValue file.name loc :: acc
            Nothing ->
                acc
    ) [] files

calculateComments: List MyFile -> List Metric.Value
calculateComments files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = ASTHelper.processRawFile ast
                    comments = List.foldl numberOfCommentedLines 0 (ASTHelper.getCommentLines processedFile) |> toFloat
                in
                     initValue file.name comments :: acc
            Nothing ->
                acc
    ) [] files

calculateNoD: List MyFile -> List Metric.Value
calculateNoD files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = ASTHelper.processRawFile ast
                    nod = ASTHelper.numberOfDeclarations processedFile |> toFloat
                in
                     initValue file.name nod :: acc
            Nothing ->
                acc
    ) [] files

calculateNoF: List MyFile -> List Metric.Value
calculateNoF files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = ASTHelper.processRawFile ast
                    nof = ASTHelper.numberOfFunctions processedFile |> toFloat
                in
                     initValue file.name nof :: acc
            Nothing ->
                acc
    ) [] files

calculateNoA: List MyFile -> List Metric.Value
calculateNoA files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = ASTHelper.processRawFile ast
                    noa = ASTHelper.numberOfTypeAliases processedFile |> toFloat
                in
                     initValue file.name noa :: acc
            Nothing ->
                acc
    ) [] files

calculateNoT: List MyFile -> List Metric.Value
calculateNoT files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = ASTHelper.processRawFile ast
                    not = ASTHelper.numberOfTypes processedFile |> toFloat
                in
                     initValue file.name not :: acc
            Nothing ->
                acc
    ) [] files


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
            names = List.map (\file -> file.name) files
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
                                        div[ class "card" ][
                                             table[ style "text-align" "left", style "width" "100%"][
                                                List.map (\val -> th[][ text val ]) ("Modules" :: (keys model.metrics)) |> tr[],
                                                ---------
                                                List.map (localTableContent model.metrics) names |> tbody[]
                                                ---------
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

localTableContent: Dict String Metric -> String -> Html msg
localTableContent metrics name  =
    List.foldl(\metric acc ->
        case find(\val -> if(val.parentDeclaration == name) then True else False) (Metric.getValues metric) of
            Just num ->
                acc ++ [ td[][ text (num.value |> toString) ]] 
            Nothing ->
                acc ++ [ td [][ text "--" ] ]
    ) [td[] [ text name ]] (values metrics) |> tr[]
    
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