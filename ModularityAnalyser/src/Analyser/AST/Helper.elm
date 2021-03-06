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
import Dict exposing (Dict, fromList, keys)
import Html exposing (a, Html, div, text)
import Dict exposing (Dict, map, values)
import List.Extra exposing (find)
import Analyser.AST.Declaration exposing (Declaration_)
import Set exposing (Set, member, fromList, toList, diff)
import Analyser.File exposing (File_)


--pipeline that calculates the relationship between modules, metrics CA CE and Instability, even LS and NOL
mainPipeline: List Declaration_ -> RawFile -> List RawFile -> List Declaration_
mainPipeline declarations ast rawfiles =
    --omg
    let
        --set of all declarations belonging to currently processed module so they can be disregarded
        set = getModuleDeclarationsList(declarations)
        --set of all module names that are not external - project modules
        moduleSet = Set.fromList (List.map(\val -> (String.join "." (moduleName val))) rawfiles)
        --set of all modules that are imported to the module and are exposing All (need to check into these modules to cross reference)
        moduleNameList = 
            List.filterMap(\imp ->
                case imp.exposingList of
                    Just node ->
                        case value node of
                            All _ ->
                                case Set.member (String.join "." (value imp.moduleName)) moduleSet of
                                    True ->
                                        Just imp
                                    False ->
                                        Nothing
                            _ ->
                                Nothing
                    Nothing ->
                        -- if exposes is not defined, still can be added to the list if project module
                        case Set.member (String.join "." (value imp.moduleName)) moduleSet of
                            True ->
                                Just imp
                            False ->
                                Nothing
            ) (imports ast)
        --list of all imports that are not exposing All - can check if module exposes a certain declaration. 
        --Might be an issue with aliases here (import x as y) - will need to be fixed in the future
        importList = 
            List.filterMap(\imp ->
                case imp.exposingList of
                    Just node ->
                        case value node of
                            All _ ->
                                Nothing
                            Explicit _ ->
                                case Set.member (String.join "." (value imp.moduleName)) moduleSet of
                                    True ->
                                        Just imp
                                    False ->
                                        Nothing
                    Nothing ->
                        -- if exposes is not defined, still can be added to the list if project module
                        case Set.member (String.join "." (value imp.moduleName)) moduleSet of
                            True ->
                                Just imp
                            False ->
                                Nothing
            ) (imports ast)
    in
        -- Debug.log (moduleNameList |> Debug.toString)
        --Debug.log ( (List.map(\node -> value node.moduleName)) |> Debug.toString)
        -- Debug.log (moduleSet |> Debug.toString)
        List.map(\dec ->
            { dec | calledModules =
                List.foldl(\val acc ->
                    case Set.member val set of
                        True ->
                            --we skip this declaration because it's a part of this module!
                            acc
                        False ->
                            let
                                --first check if any name matches (Module.blabla.blabla)
                                --import module x | import module x as y exposing (abc, abcd, abdsd) ...
                                exposingNameCheck = 
                                    find(\mod ->  
                                        checkDeclarationImportMatch val mod     
                                    )(importList)
                            in
                                case exposingNameCheck of
                                    Just mod ->
                                        String.join "." (value mod.moduleName) :: acc
                                    Nothing ->
                                        --if name doesn't match, we need to check if any declaration is being explicilty exposed
                                        --import module x exposing (a, b, c)
                                        let
                                            exposingCheck = 
                                                find(\mod ->
                                                    case mod.exposingList of
                                                        Just expList ->
                                                            exposingExposes val (value expList)
                                                        Nothing ->
                                                            False
                                                ) (importList)
                                        in
                                            case exposingCheck of
                                                Just mod ->
                                                    String.join "." (value mod.moduleName) :: acc
                                                Nothing ->
                                                    --if this doesn't match, we need to search through imports that expose All
                                                    let
                                                        interfaceNameCheck =
                                                            --first we try to match the name again
                                                            find(\mod ->
                                                                let
                                                                    modName = String.join "." (value mod.moduleName)
                                                                in  
                                                                    checkDeclarationImportMatch val mod 
                                                            ) moduleNameList
                                                    in
                                                        case interfaceNameCheck of
                                                            Just m ->
                                                                String.join "." (value m.moduleName) :: acc
                                                            Nothing ->
                                                                --If name doesn't match, we start matching through all exposed declarations one import by one untill
                                                                --we find a match
                                                                let
                                                                    interfaceCheck =
                                                                        find(\mod ->
                                                                            let
                                                                                modName = String.join "." (value mod.moduleName)
                                                                            in
                                                                                case getModuleByName modName rawfiles of
                                                                                    Just m ->
                                                                                        interfaceExposes val (build m)
                                                                                    Nothing ->
                                                                                        False
                                                                            
                                                                        ) moduleNameList
                                                                in
                                                                    case interfaceCheck of
                                                                        Nothing ->
                                                                            --this will happen only for let variables | exports from external modules | Types as Just, Maybe etc...
                                                                            acc
                                                                        Just m ->
                                                                            String.join "." (value m.moduleName) :: acc
                                            --"Different module" 
                ) [] dec.calledDecl
            }
        ) declarations

checkDeclarationImportMatch: String -> Import -> Bool
checkDeclarationImportMatch name imp =
    let
        split = String.split "." name
    in
        if (List.length split) < 2 then
            False
        else
            let
                val = List.drop 1 (List.reverse split)
                declName = String.join "." (List.reverse val)  
            in
                case imp.moduleAlias of
                    Just alias_ ->
                        if declName == (String.join "." (value alias_)) then
                            True
                        else
                            if declName == (String.join "." (value imp.moduleName)) then
                                True
                            else
                                False
                    Nothing ->    
                        if declName == (String.join "." (value imp.moduleName)) then
                            True
                        else
                            False
                    
        


getModuleDeclarationsList: List Declaration_ -> Set String
getModuleDeclarationsList declarations =
    Set.fromList (List.map(\val -> val.name) declarations)

getModuleByName: String -> List RawFile -> Maybe RawFile
getModuleByName name list =
    --set full module name, with the entire path and so on..
    let
        find =
            List.Extra.find(\file ->
                if (String.join "." (moduleName file)) == name then
                    True
                else
                    False
            ) list
    in
        case find of
            Nothing ->
                Nothing
            Just raw ->
                Just raw

addCalledByModules: List File_ -> List File_
addCalledByModules files =
    List.foldl(\file1 acc1 ->
        {
            file1 | calledByModules = 
                Set.fromList (List.foldl(\file2 acc2 ->
                    case file1.ast of
                        Just ast ->
                            if Set.member (getModuleNameRaw ast) (Set.fromList (keys file2.calledModules)) then
                                case file2.ast of
                                    Just ast2 ->
                                        (getModuleNameRaw ast2) :: acc2
                                    Nothing ->
                                        acc2
                            else
                                acc2
                        Nothing ->
                            acc2               
                ) [] files)
        } :: acc1
    ) [] files



    --     List.foldl(\file acc ->
    --         acc
    --         ++
    --         case file.ast of
    --             Just ast ->
    --                 List.foldl(\file2 acc2 ->
    --                     { file | calledByModules =
    --                         case file2.ast of
    --                             Just ast2 ->
    --                                 Set.foldl(\moduleName acc3 -> 
    --                                     if(getModuleNameRaw ast) == moduleName then
    --                                         Set.insert (getModuleNameRaw ast2) acc3
    --                                     else
    --                                         acc3
    --                                 ) file.calledByModules file2.calledModules
    --                             Nothing ->
    --                                 file.calledByModules
    --                     } :: acc2
    --                 ) [] files
    --             Nothing ->
    --                 acc
    --     )[] files

        -- List.foldl(\file2 acc ->
        --     acc
        --     ++
        --     List.foldl(\mod acc2 -> 
        --         case file2.ast of
        --             Just ast2 ->
        --                 if(Helper.getModuleNameRaw ast) == mod then
        --                     (Helper.getModuleNameRaw ast2) :: acc2
        --                 else
        --                     acc2
        --     ) [] file2.calledModules
        -- ) [] files

findUnusedImports: List Import -> List String -> List String
findUnusedImports allImports usedImports =
    let
        importList = List.map(\imp -> String.join "." (value imp.moduleName)) allImports
    in
        Set.toList (Set.diff (Set.fromList importList) (Set.fromList usedImports))

interfaceExposes: String -> Interface -> Bool
interfaceExposes name int =
    --String.split  "." name
    --last (tail) compare if all remaining dont fit any interface name/alias!!
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
    --last (tail) compare if all remaining dont fit any interface name/alias!!
    --maybe try using only interface? wont work, wonder how to find if stuff has alias when exposing?? surely can be done
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

importsList: RawFile -> List Import
importsList raw =
    imports raw

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

getModuleNameRaw: RawFile -> String
getModuleNameRaw raw =
    String.join "." (moduleName raw)

moduleNameToString: RawFile -> String
moduleNameToString raw =
    String.join "." (moduleName raw)

moduleNameToList: ModuleName -> List String
moduleNameToList modules =
    modules

moduleNameToString2: ModuleName -> String
moduleNameToString2 name =
    String.join "." name

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


