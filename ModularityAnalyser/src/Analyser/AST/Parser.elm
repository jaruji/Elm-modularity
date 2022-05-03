module Analyser.AST.Parser exposing (..)

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


parseDeclaration: Declaration -> List String
parseDeclaration dec =
    case dec of
        FunctionDeclaration func ->
            --{declaration, documentation, signature}
            parseFunction func
        AliasDeclaration alias_ ->
            --{documentation, name, generics, typeAnnotation}
            parseAlias alias_
        CustomTypeDeclaration type_ ->
            --{documentation, name, generics, constructors}
            parseType type_
        PortDeclaration sig ->
            --{name, typeAnnotation}
            parseSignature sig
        _ ->
            []

parseFunction: Function -> List String
parseFunction {documentation, signature, declaration} =
    parseFunctionImplementation (value declaration)

parseFunctionImplementation: FunctionImplementation -> List String
parseFunctionImplementation {name, arguments, expression} =
    parseExpression (value expression)

parseExpression: Expression -> List String
parseExpression exp =
    --giga case here
    []

parseType: Type -> List String
parseType { documentation, name, generics, constructors } =
    --flatten array here?
    -- List.foldl(\val acc ->
    --     val :: acc
    -- ) [] (List.map(\cons -> parseValueConstructor (value cons)) constructors)
    []

parseValueConstructor: ValueConstructor -> List String
parseValueConstructor { name, arguments } =
    -- List.foldl (\cons acc -> (value cons :: acc)) [] arguments
    []

parseAlias: TypeAlias -> List String
parseAlias alias_ =
    []

parseSignature: Signature -> List String
parseSignature sig =
    []