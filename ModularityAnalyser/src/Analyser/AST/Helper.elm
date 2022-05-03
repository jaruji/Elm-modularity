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
import Analyser.AST.Declaration as Dec exposing (Declaration)

getImports: RawFile -> List String
getImports ast =
    List.map getModuleName (imports ast)

getModuleName: Import -> String
getModuleName {moduleName, moduleAlias, exposingList} =
    String.join "." (moduleNameToList (value moduleName))

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

getNodeLOC: Node a -> Int
getNodeLOC node =
    let
        interval = getNodeRange node
    in
        Tuple.second interval - Tuple.first interval

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

-- getExposedDeclarations: File -> List String
-- getExposedDeclarations {moduleDefinition, imports, declarations, comments} =
--     let
--         moduleDef = value moduleDefinition
--     in 
--         case exposingList moduleDef of
--             All _ ->
--                 ["all"]
--             Explicit list ->
--                 List.foldl (\topLevelExpose acc -> 
--                     case value topLevelExpose of
--                         InfixExpose val ->
--                             val :: acc
--                         FunctionExpose val ->
--                             val :: acc
--                         TypeOrAliasExpose val ->
--                             val :: acc
--                         TypeExpose val ->
--                             val.name :: acc
--                 ) [] list
    
-- getAllDeclarations: File -> List String
-- getAllDeclarations {moduleDefinition, imports, declarations, comments} =
--     List.foldl(\dec acc ->
--         case value dec of
--             FunctionDeclaration {declaration, documentation, signature} ->
--                 case signature of
--                     Just val ->
--                         acc
--                     Nothing ->
--                         acc
--             AliasDeclaration {documentation, name, generics, typeAnnotation} ->
--                 (value name) :: acc
--             CustomTypeDeclaration {documentation, name, generics, constructors} ->
--                 (value name) :: acc
--             PortDeclaration {name, typeAnnotation} ->
--                 (value name) :: acc
--             InfixDeclaration val ->
--                 acc
--             Destructuring _ _ ->
--                 acc
--     ) [] declarations

getAllDeclarations: RawFile -> List Exposed
getAllDeclarations file =
    build file

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


