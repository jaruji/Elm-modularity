module Analyser.Metric exposing (..)

type alias Metric =
    {
        name: String,
        upperThreshold: Float,
        lowerThreshold: Float,
        metricType: MetricType,
        values: List Value
    }

type MetricType 
    = ModuleMetric
    | FunctionMetric
    | ProjectMetric

type alias Value =
    {
        value: Float,
        parentDeclaration: Maybe String
    }

init: String -> Float -> Float -> MetricType -> Metric
init string lower upper mType =
    ({ name = string, upperThreshold = upper, lowerThreshold = lower, metricType = mType, values = []})

initWithValues: String -> Float -> Float -> MetricType -> List Value -> Metric
initWithValues string lower upper mType values =
    ({ name = string, upperThreshold = upper, lowerThreshold = lower, metricType = mType, values = values})

setValues: Metric -> List Value -> Metric
setValues model values =
    ({model | values = values})

appendValues: Metric -> List Value -> Metric
appendValues model values =
    ({ model | values = values ++ model.values})