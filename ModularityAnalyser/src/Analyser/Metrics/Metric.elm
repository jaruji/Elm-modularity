module Analyser.Metrics.Metric exposing (..)
import List exposing (length)
import Dict exposing (Dict, get, foldl, map, values, keys)
import Dashboard.Components.FileSelector exposing (MyFile)
import Analyser.AST.Helper as Helper exposing (..)

type alias Metric =
    {
        name: String,
        upperThreshold: Float,
        lowerThreshold: Float,
        metricType: MetricType,
        values: List Value,
        averageValue: Float
    }

type MetricType 
    = ModuleMetric
    | FunctionMetric

type alias Value =
    {
        value: Float,
        parentDeclaration: String
    }

init: String -> Float -> Float -> MetricType -> Metric
init string lower upper mType =
    ({ name = string, upperThreshold = upper, lowerThreshold = lower, metricType = mType, values = [], averageValue = 0.0})

initWithValues: String -> Float -> Float -> MetricType -> List Value -> Metric
initWithValues string lower upper mType values =
    ({ name = string, upperThreshold = upper, lowerThreshold = lower, metricType = mType, values = values, averageValue = 0.0})

initValue: String -> Float -> Value
initValue dec val =
    ({ value = val, parentDeclaration = dec})

setValues: Metric -> List Value -> Metric
setValues model values =
    ({model | values = values})

appendValues: Metric -> List Value -> Metric
appendValues model values =
    ({ model | values = values ++ model.values})

getValues: Metric -> List Value
getValues metric =
    metric.values

averageMetric: Metric -> Float
averageMetric metric = 
    let
        sum = List.foldl(\val acc -> val.value + acc) 0 metric.values
    in
        if sum == 0 then
            0.0
        else
            sum / ((List.length(metric.values)) |> toFloat)

setAverage: Metric -> Float -> Metric
setAverage metric avg =
    ({metric | averageValue = avg})

calculateLOC: List MyFile -> List Value
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

calculateComments: List MyFile -> List Value
calculateComments files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = Helper.processRawFile ast
                    comments = List.foldl numberOfCommentedLines 0 (Helper.getCommentLines processedFile) |> toFloat
                in
                     initValue file.name comments :: acc
            Nothing ->
                acc
    ) [] files

calculateNoD: List MyFile -> List Value
calculateNoD files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = Helper.processRawFile ast
                    nod = Helper.numberOfDeclarations processedFile |> toFloat
                in
                     initValue file.name nod :: acc
            Nothing ->
                acc
    ) [] files

calculateNoF: List MyFile -> List Value
calculateNoF files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = Helper.processRawFile ast
                    nof = Helper.numberOfFunctions processedFile |> toFloat
                in
                     initValue file.name nof :: acc
            Nothing ->
                acc
    ) [] files

calculateNoA: List MyFile -> List Value
calculateNoA files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = Helper.processRawFile ast
                    noa = Helper.numberOfTypeAliases processedFile |> toFloat
                in
                     initValue file.name noa :: acc
            Nothing ->
                acc
    ) [] files

calculateNoT: List MyFile -> List Value
calculateNoT files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = Helper.processRawFile ast
                    not = Helper.numberOfTypes processedFile |> toFloat
                in
                     initValue file.name not :: acc
            Nothing ->
                acc
    ) [] files

numberOfCommentedLines: (Int, Int) -> Int -> Int
numberOfCommentedLines (start, end) acc =
    acc + end - start + 1