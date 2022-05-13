module Analyser.Metrics.Metric exposing (..)
import List exposing (length)
import Dict exposing (Dict, get, foldl, map, values, keys, fromList)
import Analyser.File exposing (File_)
import Analyser.AST.Helper as Helper exposing (..)
import Round exposing (round, roundNum)
import Set exposing (size, member, union)
import Analyser.Metrics.Thresholds exposing (..)


decimals: Int
decimals =
    4

type alias Metric =
    {
        name: String,
        upperThreshold: Float,
        lowerThreshold: Float,
        description: String,
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
    ({ name = string, upperThreshold = upper, lowerThreshold = lower, description = "", metricType = mType, values = [], averageValue = 0.0})

initWithValues: String -> Float -> Float -> String -> MetricType -> List Value -> Metric
initWithValues string lower upper desc mType values =
    ({ name = string, upperThreshold = upper, lowerThreshold = lower, description = desc, metricType = mType, values = values, averageValue = 0.0})

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
            Round.roundNum decimals (sum / ((List.length(metric.values)) |> toFloat))

setAverage: Metric -> Float -> Metric
setAverage metric avg =
    ({metric | averageValue = avg})

getNonEmptyLineCount: String -> Int
getNonEmptyLineCount content =
    let
        parsedString = String.lines content
        filtered = 
            List.filter(\line -> 
                if String.length line == 0 then
                    False
                else
                    True
            ) parsedString
    in
        (List.length filtered)

calculateDeclarationOutdegree: List File_ -> List File_
calculateDeclarationOutdegree files =
    files


calculateLOC: List File_ -> List Value
calculateLOC files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    loc = (getNonEmptyLineCount file.content) |> toFloat
                in
                     initValue (getModuleNameRaw ast) loc :: acc
            Nothing ->
                acc
    ) [] files

calculateCE: List File_ -> List (String, Float)
calculateCE files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    ce = Dict.size file.calledModules
                in
                    (getModuleNameRaw ast, ce |> toFloat) :: acc
            Nothing ->
                acc
    ) [] files

calculateCED: List File_ -> List (String, Float)
calculateCED files =
    --CE BASED ON DECLARATION CALL COUNT AND NOT MODULE CALL COUNT:
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    ce = 
                        List.foldl(\val sum -> 
                            if List.length val.calledModules > 0 then
                                sum + 1
                            else
                                sum
                        ) 0 file.declarations |> toFloat
                in
                    (getModuleNameRaw ast, ce) :: acc
            Nothing ->
                acc
    ) [] files

calculateCA: List File_ -> List (String, Float)
calculateCA files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    moduleName = Helper.getModuleNameRaw ast
                    ca = 
                        List.foldl(\val sum -> 
                            if (Dict.member moduleName val.calledModules) then
                                sum + 1
                            else
                                sum
                        ) 0 files
                in
                    (getModuleNameRaw ast, ca |> toFloat) :: acc
            Nothing ->
                acc
    ) [] files

calculateCAD: List File_ -> List (String, Float)
calculateCAD files =
-- CA BASED ON DECLARATION CALL COUNT AND NOT MODULE CALL COUNT:
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    moduleName = Helper.getModuleNameRaw ast
                    ca = 
                        List.foldl(\val sum -> 
                            sum 
                            +
                            List.foldl(\val2 sum2 ->
                                if List.member moduleName val2.calledModules then
                                    sum2 + 1
                                else
                                    sum2
                            ) 0 val.declarations
                            
                        ) 0 files |> toFloat
                in
                    (getModuleNameRaw ast, ca) :: acc
            Nothing ->
                acc
    ) [] files

--need to figure out how to match boilerplate pattern

calculateComments: List File_ -> List Value
calculateComments files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = Helper.processRawFile ast
                    comments = List.foldl numberOfCommentedLines 0 (Helper.getCommentLines processedFile) |> toFloat
                in
                     initValue (getModuleNameRaw ast) comments :: acc
            Nothing ->
                acc
    ) [] files

calculateNoD: List File_ -> List Value
calculateNoD files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = Helper.processRawFile ast
                    nod = Helper.numberOfDeclarations processedFile |> toFloat
                in
                     initValue (getModuleNameRaw ast) nod :: acc
            Nothing ->
                acc
    ) [] files

calculateNoF: List File_ -> List Value
calculateNoF files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = Helper.processRawFile ast
                    nof = Helper.numberOfFunctions processedFile |> toFloat
                in
                     initValue (getModuleNameRaw ast) nof :: acc
            Nothing ->
                acc
    ) [] files

calculateNoA: List File_ -> List Value
calculateNoA files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = Helper.processRawFile ast
                    noa = Helper.numberOfTypeAliases processedFile |> toFloat
                in
                     initValue (getModuleNameRaw ast) noa :: acc
            Nothing ->
                acc
    ) [] files

calculateNoT: List File_ -> List Value
calculateNoT files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                let
                    processedFile = Helper.processRawFile ast
                    not = Helper.numberOfTypes processedFile |> toFloat
                in
                    initValue (getModuleNameRaw ast) not :: acc
            Nothing ->
                acc
    ) [] files

calculateNoL: List File_ -> List Value
calculateNoL files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                    let
                        nol = List.foldl(\dec sum -> sum + dec.lambdaCount) 0 file.declarations |> toFloat
                    in
                        initValue (getModuleNameRaw ast) nol :: acc
            Nothing ->
                acc
    ) [] files

calculateLS: List File_ -> List Value
calculateLS files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                    let
                        lambdaLoc = List.foldl(\dec sum -> sum + dec.lambdaLines) 0 file.declarations |> toFloat
                        loc = (getNonEmptyLineCount file.content) |> toFloat
                        ls = Round.roundNum decimals (lambdaLoc / loc)
                    in
                        initValue (getModuleNameRaw ast) ls :: acc
            Nothing ->
                acc
    ) [] files

calculateInstability: List (String, Float) -> List (String, Float) -> List Value
calculateInstability caList ceList =
    List.map2(\ca ce -> 
        let
            caVal = Tuple.second ca
            ceVal = Tuple.second ce

        in
            if ceVal + caVal == 0 then
                initValue (Tuple.first ca) 0
            else
                initValue (Tuple.first ca) (roundNum decimals (ceVal/(ceVal + caVal)))
    ) caList ceList

calculateCBM: List File_ -> List Value
calculateCBM files =
    List.foldl(\file acc -> 
        case file.ast of
            Just ast ->
                    let
                        set = Set.union file.calledByModules (Set.fromList (keys file.calledModules))
                    in
                        initValue (getModuleNameRaw ast) ((Set.size set) |> toFloat) :: acc
            Nothing ->
                acc
    ) [] files



calculateMetrics: List File_ -> Dict String Metric
calculateMetrics files =
    let
        ca = calculateCA files
        ce = calculateCE files
        cad = calculateCAD files
        ced = calculateCED files
    in
        Dict.map(\_ val -> 
            setAverage val (averageMetric val)
        ) 
        (
            fromList[ 
                ("LOC", initWithValues "LOC" 0 0 "Lines of code metric displays the count of the real lines of code, without empty lines." ModuleMetric (calculateLOC files)),
                -- ("Comments", initWithValues "Comments" 0 0 ModuleMetric (calculateComments files)), 
                ("NoF", initWithValues "NoF" 0 0 "Number of functions metric represents the number of function declarations in a module." ModuleMetric (calculateNoF files)), 
                ("NoD", initWithValues "NoD" 0 0 "Number of declarations metric represents the number of all declarations in a module." ModuleMetric (calculateNoD files)), 
                ("NoT", initWithValues "NoT" 0 0 "Number of types metric represents the number of type declarations in a module." ModuleMetric (calculateNoT files)), 
                ("NoA", initWithValues "NoA" 0 0 "Number of aliases metric represents the number of type alias declarations in a module." ModuleMetric (calculateNoA files)), 
                ("CA", initWithValues "CA" 0 0 "Afferent coupling metric represents the number of modules that are calling a specific module." ModuleMetric (List.map(\val -> initValue (Tuple.first val) (Tuple.second val)) ca)), 
                ("CE", initWithValues "CE" 0 0 "Efferent coupling metric represents the number of modules a specific module is calling." ModuleMetric (List.map(\val -> initValue (Tuple.first val) (Tuple.second val)) ce)), 
                ("CA(w)", initWithValues "CA(w)" 0 0 "Weighted afferent metric represents the number of external declarations that are calling a specific module." ModuleMetric (List.map(\val -> initValue (Tuple.first val) (Tuple.second val)) cad)), 
                ("CE(w)", initWithValues "CE(w)" 0 0 "Weighted efferent coupling metric represents the number of external declarations a specific module is calling." ModuleMetric (List.map(\val -> initValue (Tuple.first val) (Tuple.second val)) ced)), 
                ("Instability", initWithValues "Instability" 0 0 "Instability metric is counted from CA and CE, it represents a module's resistance to change. It's values are in the interval of 0 to 1." ModuleMetric (calculateInstability ca ce)),
                ("Instability(w)", initWithValues "Instability(w)" 0 0 "Weighted instability metric is counted from CA(w) and CE(w), it represents a module's resistance to change. It's values are in the interval of 0 to 1." ModuleMetric (calculateInstability cad ced)),
                ("NoL", initWithValues "NoL" 0 0 "Number of lambdas metric represents the number of lambda function declarations in a module." ModuleMetric (calculateNoL files)),
                ("LS", initWithValues "LS" 0 0 "Lambda score metric represents the ratio of lambda function lines to all lines of code (LOC metric)." ModuleMetric (calculateLS files)),
                ("CBM", initWithValues "CBM" 0 0 "Coupling between modules metric represents the number of unique relationships a module has with other modules. It is the sum of CA and CE, because cycles are not allowed in Elm." ModuleMetric (calculateCBM files))
            ]
        )


--infix, outfix also possible
--operatorCount, --operandCount
--SCCS - interesting concept

numberOfCommentedLines: (Int, Int) -> Int -> Int
numberOfCommentedLines (start, end) acc =
    acc + end - start + 1