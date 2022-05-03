module Analyser.AST.Declaration exposing (..)

type alias Declaration =
    {
        name: String,
        decType: Type,
        depth: Int,
        calledModules: List (List String),
        uniqueCalledModulesCount: Int,
        lambdaCount: Int,
        debugString: String
    }

init: String -> Type -> Int -> List (List String) -> Int -> Int -> Declaration
init name decType d cm ucmc lc =
    (Declaration name decType d cm ucmc lc "")

type Type
    = Function
    | Alias
    | Type
    | Other