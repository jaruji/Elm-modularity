module Analyser.AST.Parser exposing (..)

import Analyser.AST.Helper exposing (getFunctionLOC, getNodeLOC, moduleNameToString2, getNodeRange)
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
import Elm.Syntax.TypeAnnotation exposing (..)
import Analyser.AST.Declaration as Custom exposing (Declaration_)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import List exposing (length)
import Analyser.AST.Boilerplate as Boilerplate exposing (Boilerplate_, default, init, Type_(..))

--best way to store used imports?
--Dict ModuleName (List Import_)

parseRawFile: RawFile -> List Custom.Declaration_
parseRawFile raw =
    parseFile (process Processing.init raw)

parseFile: File -> List Declaration_
parseFile {moduleDefinition, imports, declarations, comments} =
    let
        dec = Custom.init "" Custom.Default 0 [] 0 0
    in
        List.foldl(\node acc -> parseDeclaration dec (value node) :: acc) [] declarations

parseDeclaration: Declaration_ -> Declaration -> Declaration_
parseDeclaration dec val =
    case val of
        FunctionDeclaration func ->
            --{declaration, documentation, signature}
            parseFunction { dec | decType = Custom.Function, lineCount = getFunctionLOC func } func
        AliasDeclaration alias_ ->
            --{documentation, name, generics, typeAnnotation}
            parseAlias { dec | decType = Custom.Alias } alias_
        CustomTypeDeclaration type_ ->
            --{documentation, name, generics, constructors}
            parseType { dec | decType = Custom.Type } type_
        PortDeclaration sig ->
            --{name, typeAnnotation}
            parseSignature { dec | decType = Custom.Port } sig
        _ ->
            dec

parseFunction: Declaration_ -> Function -> Declaration_
parseFunction dec {documentation, signature, declaration} =
    case signature of
        Nothing ->
            parseFunctionImplementation dec (value declaration)
        Just sig ->
            parseFunctionImplementation (parseSignature dec (value sig)) (value declaration)

parseFunctionImplementation: Declaration_ -> FunctionImplementation -> Declaration_
parseFunctionImplementation dec {name, arguments, expression} =
    parseExpression (value expression) { dec | name = value name } 

parseExpression: Expression -> Declaration_ -> Declaration_
parseExpression exp dec =
    case exp of
        UnitExpr ->
            dec
        Application list ->
            --(List(Node Expression))
            List.foldl(\val acc -> parseExpression (value val) acc) dec list
        OperatorApplication val infixDir exp1 exp2 ->
            --(String InfixDirection (Node Expression)(Node Expression))
            parseExpression (value exp1) dec |> parseExpression (value exp2)
        FunctionOrValue modName val ->
            --(ModuleName String)
            --this is important for me!
            {
                dec |  
                    calledDecl = 
                        if List.length modName == 0 then
                            val :: dec.calledDecl
                        else
                            ((String.join "." modName) ++ "." ++ val) :: dec.calledDecl
                --, debugString = "KUKLE"
            }
        IfBlock exp1 exp2 exp3 ->
            --(Node Expression Node Expression Node Expression)
            parseExpression (value exp1) dec |> parseExpression (value exp2) |> parseExpression (value exp3)
        PrefixOperator val ->
            dec
        Elm.Syntax.Expression.Operator val ->
            dec
        Integer int ->
            dec
        Hex int ->
            dec
        Floatable float ->
            dec
        Negation exp1 ->
            parseExpression (value exp1) dec
        Literal val ->
            dec
        CharLiteral char ->
            dec
        TupledExpression list ->
            --cooked
            List.foldl(\val acc -> parseExpression (value val) acc) dec list
        ParenthesizedExpression exp1 ->
            parseExpression (value exp1) dec
        LetExpression letBlock ->
            parseLetBlock dec letBlock
        CaseExpression caseBlock ->
            parseCaseBlock dec caseBlock
        LambdaExpression lambda ->
            parseLambda ({dec | lambdaCount = dec.lambdaCount + 1}) lambda
        RecordExpr list ->
            dec
        ListExpr list ->
            List.foldl(\val acc -> parseExpression (value val) acc) dec list
        RecordAccess exp1 val ->
            parseExpression (value exp1) dec
        RecordAccessFunction val ->
            dec
        RecordUpdateExpression _ list ->
            List.foldl(\val acc -> parseRecordSetter acc (value val)) dec list
        GLSLExpression val ->
            dec

parseLetBlock: Declaration_ -> LetBlock -> Declaration_
parseLetBlock dec { declarations, expression } =
    List.foldl(\val acc -> parseLetDeclaration acc (value val)) dec declarations |> parseExpression (value expression)

parseLetDeclaration: Declaration_ -> LetDeclaration -> Declaration_
parseLetDeclaration dec ld =
    case ld of
        LetFunction f ->
            parseLetFunction dec f
        LetDestructuring pattr expr ->
            parsePattern dec (value pattr) |> parseExpression (value expr)

-- parseLetFunction: Declaration_ -> Function -> Declaration_
-- parseLetFunction dec {documentation, signature, declaration} =
--     case signature of
--         Nothing ->
--             parseFunctionImplementation dec (value declaration)
--         Just sig ->
--             parseFunctionImplementation (parseSignature dec (value sig)) (value declaration)


parseCaseBlock: Declaration_ -> CaseBlock -> Declaration_
parseCaseBlock dec { expression, cases } =
    List.foldl(\val acc -> parseCase acc val) dec cases |> parseExpression (value expression)

parseCase: Declaration_ -> Case -> Declaration_
parseCase dec (pattr, expr) =
    parsePattern {dec | caseCount = dec.caseCount + 1} (value pattr) |> parseExpression (value expr)

parseLambda: Declaration_ -> Lambda -> Declaration_
parseLambda dec { args, expression } =
    let
        lambdaLines = getNodeLOC expression + List.foldl(\val acc -> acc + getNodeLOC val) 0 args 
    in
        List.foldl(\val acc -> parsePattern acc (value val)) { dec | lambdaLines = lambdaLines } args |> parseExpression (value expression)

parseRecordSetter: Declaration_ -> RecordSetter -> Declaration_
parseRecordSetter dec (val, expr) =
    parseExpression (value expr) dec

parseType: Declaration_ -> Type -> Declaration_
parseType dec { documentation, name, generics, constructors } =
    let
        loc = List.foldl(\node acc -> acc + getNodeLOC node) 0 constructors
    in
        List.foldl(\val acc -> parseValueConstructor acc (value val)) { dec | name = value name, lineCount = loc } constructors

parseValueConstructor: Declaration_ -> ValueConstructor -> Declaration_
parseValueConstructor dec { name, arguments } =
    -- List.foldl (\cons acc -> (value cons :: acc)) [] arguments
    List.foldl(\val acc -> parseTypeAnnotation acc (value val)) dec arguments

parseTypeAnnotation: Declaration_ -> TypeAnnotation -> Declaration_
parseTypeAnnotation  dec typeA =
    case typeA of
        Typed node list ->
            List.foldl(\val acc -> parseTypeAnnotation acc (value val)) {
                dec | calledDecl = 
                    let
                        modName = (Tuple.first (value(node)))
                        val = (Tuple.second (value(node))) 
                    in
                        if List.length modName == 0 then
                            val :: dec.calledDecl
                        else
                            ((String.join "." modName) ++ "." ++ val) :: dec.calledDecl
                    -- dec.debugString 
                    -- ++ 
                    -- "ModuleName: "
                    -- ++
                    -- Debug.toString (Tuple.first (value(node))) 
                    -- ++
                    -- "String: "
                    -- ++
                    -- Debug.toString (Tuple.second (value(node))) 
                    -- ++ 
                    -- "||"
            } list
        Tupled list ->
            List.foldl(\val acc -> parseTypeAnnotation acc (value val)) dec list
        Record rec ->
            parseRecordDefinition dec rec
        GenericRecord node rec ->
            parseRecordDefinition dec (value rec)
        FunctionTypeAnnotation typeA1 typeA2 ->
            parseTypeAnnotation (parseTypeAnnotation dec (value typeA1)) ( value typeA2 )
        _ ->
            dec

parseRecordDefinition: Declaration_ -> RecordDefinition -> Declaration_
parseRecordDefinition dec rd =
    List.foldl(\val acc -> parseRecordField acc (value val)) dec rd

parseRecordField: Declaration_ -> RecordField -> Declaration_
parseRecordField dec (string, typeA) =
    parseTypeAnnotation dec (value typeA)
    

parseAlias: Declaration_ -> TypeAlias -> Declaration_
parseAlias dec {documentation, name, generics, typeAnnotation} =
    parseTypeAnnotation { dec | name = value name, lineCount = getNodeLOC typeAnnotation } (value typeAnnotation)

parseLetFunction: Declaration_ -> Function -> Declaration_
parseLetFunction dec {documentation, signature, declaration} =
    case signature of
        Nothing ->
            parseLetFunctionImplementation dec (value declaration)
        Just sig ->
            parseLetFunctionImplementation (parseLetSignature dec (value sig)) (value declaration)

parseLetFunctionImplementation: Declaration_ -> FunctionImplementation -> Declaration_
parseLetFunctionImplementation dec {name, arguments, expression} =
    parseExpression (value expression) dec

parseLetSignature: Declaration_ -> Signature -> Declaration_
parseLetSignature dec {name, typeAnnotation} =
    parseTypeAnnotation dec (value typeAnnotation)

parseSignature: Declaration_ -> Signature -> Declaration_
parseSignature dec {name, typeAnnotation} =
    parseTypeAnnotation {dec | name = value name} (value typeAnnotation)

parsePattern: Declaration_ -> Pattern -> Declaration_ 
parsePattern dec patt =
    --moduleNames: Pattern -> List ModuleName --this is useful for me
    let
        moduleNameList = moduleNames patt
    in
        if List.length moduleNameList == 0 then
            dec
        else
            --THIS IS QUESTIONABLE, VERIFY THIS
            { dec | calledDecl = dec.calledDecl ++ List.filter(\val -> if val == "" then False else True ) (flattenModuleNameList moduleNameList) }

flattenModuleNameList: List ModuleName -> List String
flattenModuleNameList list =
    List.foldl (\val acc -> (String.join "." val) :: acc) [] list
    -- case pat of
    --     TuplePattern list ->
    --         dec
    --     RecordPattern list ->
    --         dec
    --     UnConsPattern pat1 pat2 ->
    --         dec
    --     ListPattern list ->
    --         dec
    --     NamedPattern qNameRef list ->
    --         dec
    --     AsPattern pat1 node ->
    --         dec
    --     ParenthesizedPattern pat1 ->
    --         dec
    --     _ ->
    --         dec
    
-- parseQNameRef: Declaration_ -> QualifiedNameRef -> Declaration_
-- parseQNameRef dec qNameRef =
--     dec

findBoilerplateRaw: RawFile -> List Boilerplate_
findBoilerplateRaw raw =
    findBoilerplate (process Processing.init raw)

findBoilerplate: File -> List Boilerplate_
findBoilerplate {moduleDefinition, imports, declarations, comments} =
   locateBoilerplateTypes declarations

locateBoilerplateTypes: List (Node Declaration) -> List Boilerplate_
locateBoilerplateTypes decNodeList =
    List.foldl(\val acc ->
        let
            dec = value val
        in
            case dec of
                CustomTypeDeclaration type_ ->
                    acc ++ (List.foldl(\node acc2 -> acc2 ++ listBoilerplateTypes (value node)) [] type_.constructors)
                _ ->
                    acc
    ) [] decNodeList

listBoilerplateTypes: ValueConstructor -> List Boilerplate_
listBoilerplateTypes val =
    case List.length val.arguments of
        0 ->
            []
        _ ->
            List.filterMap (\node -> checkBoilerPlateTypeAnnotation (value val.name) (value node)) val.arguments

checkBoilerPlateTypeAnnotation: String -> TypeAnnotation -> Maybe Boilerplate_
checkBoilerPlateTypeAnnotation name ta =
    case ta of
        Typed node1 node2 ->
            let
                typed = value node1
            in
                Just (Boilerplate.init (getNodeRange node1) Boilerplate.Type_ (name) (
                    let
                        nameToStr = moduleNameToString2 (Tuple.first typed)
                    in
                    if String.length nameToStr == 0 then
                        [ Tuple.second typed ]
                    else
                        [nameToStr, Tuple.second typed]
                ))
        _ ->
            Nothing
            
--locateUpdate: List Declaration
