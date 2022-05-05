module Analyser.AST.Helper exposing (..)

import Elm.RawFile exposing (..)
import Elm.Interface exposing (..)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Import exposing (..)
import Elm.Syntax.Node exposing (..)
import Elm.Syntax.File exposing (..)
import Elm.Processing as Processing exposing (..)
import Elm.Writer exposing (..)
import Elm.Syntax.Range exposing (..)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Module exposing (exposingList)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Signature exposing (..)
import Elm.Syntax.TypeAlias exposing (..)
import Elm.Syntax.Type exposing (..)
import Dict exposing (Dict, fromList)
import Html exposing (a, Html, div, text)
import Dict exposing (Dict, map, values)
import List.Extra exposing (find)
import Analyser.AST.Declaration exposing (Declaration_)


mainPipeline: List Declaration_ -> RawFile -> List Import -> List RawFile -> List Declaration_
mainPipeline declarations ast imports rawfiles =
    --TODO:
    {--
        - first filter out all declarations that belong to own file
        - then check the list of imports and split it into 2 parts -> exposes All and exposes Explicit
        - for the explicit part, use exposingExposes function and find to locate a matching Module and save it's full name into Delcaration_ mod array
        (replace the call name in calledModules array, if find is False return string "Not found")
        - next get the name of the module exposing All and find the module by name from RawFile list
        - get the interface of this module by using build on it and use interfaceExposes function to find a match, same way as with Exposing type
        - again, replace the value in calledModules array
        - we will have an array of all modules called by declaration, now we need to remove all modules that are external
        - we can count efferent coupling easily now. Afferent will need one more pass of all declarations, while creating a dictionary that represents
        - the number of times external modules call each unique declaration. Also, we will already have the number of calls for each declaration in
        calledModules field, but this is not necessary as the edges will be colored by - calculate number of decs that call each module this module
        is connected to

        Better way:
        ??
    --}
    []

exposesTypeI: String -> Interface -> Bool
exposesTypeI name int =
    let
        tmp = 
            find(\exp -> 
                case exp of
                    CustomType (string, list) ->
                        if string == name then
                            True
                        else
                            let
                                filtered = find(\val -> if val == name then True else False) list
                            in
                                case filtered of
                                    Nothing ->
                                        False
                                    _ ->
                                        True
                    _ ->
                        False
            ) int
    in
        case tmp of
            Nothing ->
                False 
            _ ->
                True

exposesAlias: String -> Exposing -> Bool
exposesAlias name exp =
    case exp of
        All range ->
            False
        Explicit list ->
            let
                tmp =
                    find(\val -> 
                        case value val of
                            TypeOrAliasExpose s ->
                                if(s == name) then True else False
                            _ ->
                                False
                    ) list
            in
                case tmp of
                    Nothing -> 
                        False 
                    _ ->
                        True

exposesType: String -> Exposing -> Bool
exposesType nm exp =
    case exp of
        All range ->
            False
        Explicit list ->
            let 
                tmp =
                    find(\val -> 
                        case value val of
                            TypeExpose {name, open} ->
                                if name == nm then True else False
                            _ ->
                                False
                    ) list
            in 
                case tmp of
                    Nothing -> 
                        False
                    _ ->
                        True

interfaceExposes: String -> Interface -> Bool
interfaceExposes name int =
    let
        tmp = 
            find(\exp -> 
                case exp of
                    CustomType (string, list) ->
                        if string == name then
                            True
                        else
                            let
                                filtered = find(\val -> if val == name then True else False) list
                            in
                                case filtered of
                                    Nothing ->
                                        False
                                    _ ->
                                        True
                    Elm.Interface.Function s ->
                        if name == s then True else False
                    Alias s ->
                        if name == s then True else False
                    _ ->
                        False
            ) int
    in
        case tmp of
            Nothing ->
                False 
            _ ->
                True


exposingExposes: String -> Exposing -> Bool 
exposingExposes nm exp =
    case exp of
        All range ->
            False
        Explicit list ->
            let 
                tmp =
                    find(\val -> 
                        case value val of
                            TypeExpose {name, open} ->
                                if nm == name then True else False
                            InfixExpose s -> 
                                if nm == s then True else False
                            FunctionExpose s ->
                                if nm == s then True else False
                            TypeOrAliasExpose s ->
                                if nm == s then True else False
                    ) list
            in 
                case tmp of
                    Nothing -> 
                        False
                    _ ->
                        True

        

getImports: RawFile -> List String
getImports ast =
    List.map getModuleName (imports ast)

getImportList: RawFile -> Dict String Exposing
getImportList raw =
--return dictionary containing a key:moduleName and Maybe List String, representing exposes
    let
        list = imports raw
    in
        Dict.fromList(
            List.foldl(\node acc -> 
                let
                    modName = node.moduleName
                in 
                    (String.join "." (value modName), getExposing node) :: acc
            ) [] list 
        )

importsToHtml: List RawFile -> Dict String Exposing -> Html msg
importsToHtml rawFiles dict = 
    Dict.map(\key val -> 
        div[][ 
            text (key ++ " : "),
            case val of
                All range ->
                    text " All"
                Explicit list ->
                    text (
                    List.foldl(\node acc -> 
                        case value node of
                            InfixExpose s ->
                                acc ++ (" " ++ s)
                            FunctionExpose s ->
                                acc ++ (" " ++ s)
                            TypeOrAliasExpose s ->
                                acc ++ (" " ++ s)
                            TypeExpose {name, open} ->
                                acc ++ (" " ++ name)
                    ) " " list )
        ]
    ) dict |> values |> div[]

exposes: String -> Exposing -> Bool
exposes func exp =
    Elm.Syntax.Exposing.exposesFunction func exp

getExposing: Import -> Exposing
getExposing imp =
    case imp.exposingList of
        Nothing ->
            Explicit []
        Just exp ->
            value exp

getNodeValue: Node a -> a
getNodeValue a =
    value a

isImportedFromModule: List String -> String -> Bool
isImportedFromModule imports dec =
    True

getModuleName: Import -> String
getModuleName {moduleName, moduleAlias, exposingList} =
    String.join "." (moduleNameToList (value moduleName))

moduleNameToString: RawFile -> String
moduleNameToString raw =
    String.join "." (moduleName raw)

moduleNameToList: ModuleName -> List String
moduleNameToList modules =
    modules

rawFileToString: RawFile -> String
rawFileToString ast =
    write (writeFile (processRawFile ast))

processRawFile: RawFile -> File
processRawFile ast =
    process Processing.init ast

joinPath: RawFile -> String
joinPath ast =
    String.join "." (moduleNameToList (moduleName ast))


getRangeLOC: Range -> Int
getRangeLOC r =
    let
        interval = getStartEnd r
    in
        Tuple.second interval - Tuple.first interval + 1

getNodeLOC: Node a -> Int
getNodeLOC node =
    let
        interval = getNodeRange node
    in
        Tuple.second interval - Tuple.first interval + 1

getNodeRange: Node a -> (Int, Int)
getNodeRange node =
    getStartEnd (range node)

getStartEnd: Range -> (Int, Int)
getStartEnd {start, end} =
    (getLocation start, getLocation end)
    

getLocation: Location -> Int
getLocation {row, column} =
    row

getCommentLines: File -> List (Int, Int)
getCommentLines {moduleDefinition, imports, declarations, comments} =
    List.map getNodeRange comments

getDeclarationLines: File -> List (Int, Int)
getDeclarationLines {moduleDefinition, imports, declarations, comments} = 
    List.map 
        (\declaration -> 
            getStartEnd (range declaration)
        ) declarations

numberOfDeclarations: File -> Int
numberOfDeclarations {moduleDefinition, imports, declarations, comments} =
   List.length declarations

numberOfFunctions: File -> Int
numberOfFunctions {moduleDefinition, imports, declarations, comments} =
    List.foldl 
        (\declaration acc ->
            case value declaration of
                FunctionDeclaration _ ->
                    acc + 1
                _ ->
                    acc
        ) 0 declarations

numberOfLambdas: File -> Int
numberOfLambdas {moduleDefinition, imports, declarations, comments} =
    0

numberOfTypes: File -> Int
numberOfTypes {moduleDefinition, imports, declarations, comments} =
    List.foldl 
        (\declaration acc ->
            case value declaration of
                CustomTypeDeclaration _ ->
                    acc + 1
                _ ->
                    acc
        ) 0 declarations

getJsonString: RawFile -> String
getJsonString raw =
    Debug.toString raw


getAllDeclarations: RawFile -> List String
getAllDeclarations file =
    let
        interface = build file
    in
        List.map(\val -> 
            case val of
                Elm.Interface.Function s ->
                    s
                CustomType (s, list) ->
                    s ++ List.foldl(\x acc ->  acc ++ " " ++ x) "" list
                Alias s ->
                    s
                Elm.Interface.Operator i ->
                    ""
        ) interface

filterFunction: Exposed -> Bool
filterFunction exposed =
    case exposed of
        Elm.Interface.Function _ ->
            True
        _ ->
            False

filterType: Exposed -> Bool
filterType exposed =
    case exposed of
        CustomType _ ->
            True
        _ ->
            False

filterAlias: Exposed -> Bool
filterAlias exposed =
    case exposed of
        Alias _ ->
            True
        _ ->
            False



getFunctionLOC: Function -> Int
getFunctionLOC {documentation, signature, declaration} =
    getNodeLOC declaration

                            
getModuleNameFromAst: RawFile -> String
getModuleNameFromAst ast =
    String.join "." (moduleName ast)

getCalledDeclarationsByModule: File -> String -> List String
getCalledDeclarationsByModule file moduleName =
    --in combination with length number of calls of declarations from a specific module
    []

couplingBetweenModules: File -> Int
couplingBetweenModules file =
    --get sum of called modules and modules in which this module is called (direction doesn't matter)
    0

boilerplateLineCount: File -> Int
boilerplateLineCount file =
    0

numberOfTypeAliases: File -> Int
numberOfTypeAliases {moduleDefinition, imports, declarations, comments} =
    List.foldl 
        (\declaration acc ->
            case value declaration of
                AliasDeclaration _ ->
                    acc + 1
                _ ->
                    acc
        ) 0 declarations



--declaration parser defined here


